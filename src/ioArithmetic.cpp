
#include <charconv>
#include <algorithm>
#include <numeric>

#include "ioArithmetic.hpp"
#include "pattern.hpp"

namespace bmath::intern {

	namespace compute {

		Result exactly_computable(const ParseView view) noexcept
		{
			if (view.tokens.find_first_of(token::character) == TokenView::npos) {
				if (view.to_string_view().find_first_of("/-.ie$") == std::string_view::npos) { //with prohibiting e, it it harder to easily surpass 2^53-1 (largest save integer stored as double)
					return Result::natural; //found no minus or i -> natural number is result
				}
				if (view.to_string_view().find_first_of("/^.e$")  == std::string_view::npos) { //e is forbidden as it otherwise enables non-integer numbers (e.g. "1e-20") 
					return Result::complex; //found no power -> complex with { a + bi | a, b in Z } is result
				}
			}
			return Result::not_exactly_computable;
		} //exactly_computable

		std::complex<double> eval_complex(ParseView view)
		{
			const auto split = [](const ParseView view, const std::size_t at) {
				return std::make_pair(view.substr(0, at), view.substr(at + 1));
			};

			do {
				if (view.size() == 1u && view.tokens.starts_with(token::imag_unit)) { 
					return std::complex<double>(0.0, 1.0); 
				}				
				if (const std::size_t sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, sum_pos);
					switch (view.chars[sum_pos]) {
					case '+': return eval_complex(lhs) + eval_complex(rhs);
					case '-': return eval_complex(lhs) - eval_complex(rhs);
					}
				}				
				if (const std::size_t product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, product_pos);
					return eval_complex(lhs) * eval_complex(rhs);
				}
				if (view.tokens.starts_with(token::unary_minus)) {
					view.remove_prefix(1u);
					return - eval_complex(view);
				}
				{
					const std::size_t not_number_pos = view.tokens.find_first_not_of(token::number);
					if (not_number_pos == TokenView::npos) {
						return parse_value(view);
					}
					else if (not_number_pos + 1u == view.size() && view.tokens.ends_with(token::imag_unit)) {
						view.remove_suffix(1u);
						return std::complex<double>(0.0, parse_value(view));
					}
				}
				if (!view.tokens.starts_with(token::open_grouping)) [[unlikely]] throw ParseFailure{ view.offset, "expected '(' or the like" };
				if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.offset + view.size(), "expected ')' or the like" };
				view.remove_prefix(1u);
				view.remove_suffix(1u);
			} while (view.size());
			throw ParseFailure{ view.offset, "run out of characters" };
		} //eval_complex

		double eval_natural(ParseView view)
		{
			const auto split = [](const ParseView view, const std::size_t at) {
				return std::make_pair(view.substr(0, at), view.substr(at + 1));
			};

			do {			
				if (const auto sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, sum_pos);
					return eval_natural(lhs) + eval_natural(rhs);
				}				
				if (const auto product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, product_pos);
					return eval_natural(lhs) * eval_natural(rhs);
				}				
				if (const auto pow_pos = find_first_of_skip_pars(view.tokens, token::hat); pow_pos != TokenView::npos) {
					const auto [lhs, rhs] = split(view, pow_pos);
					return std::pow(eval_natural(lhs), eval_natural(rhs));
				}
				if (view.tokens.find_first_not_of(token::number) == TokenView::npos) {
					return parse_value(view);
				}
				if (!view.tokens.starts_with(token::open_grouping)) [[unlikely]] throw ParseFailure{ view.offset, "expected '(' or the like" };
				if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.offset + view.size(), "expected ')' or the like" };
				view.remove_prefix(1u);
				view.remove_suffix(1u);
			} while (view.size());
			throw ParseFailure{ view.offset, "run out of characters" };
		} //eval_natural

		double parse_value(const ParseView view)
		{
			double value;
			const auto [ptr, error] = std::from_chars(view.chars, view.chars + view.size(), value);
			if (error != std::errc()) [[unlikely]] throw ParseFailure{ view.offset, "value syntax is illformed or value out of bounds" };
			if (ptr != view.chars + view.size()) [[unlikely]] throw ParseFailure{ std::size_t(view.offset + ptr - view.chars + 1u), "value syntax is illformed" };
			return value;
		} //parse_value

	} //namespace compute

	std::size_t find_first_not_arithmetic(const TokenView view) noexcept
	{
		using namespace token;
		//'\0' only as end symbol for allowed_tokens, not as part of aritmetic symbols
		const Token allowed_tokens[] = { character, comma, hat, unary_minus, sum, product, number, imag_unit, open_grouping, clse_grouping, dollar, '\0' }; 
		return view.find_first_not_of(allowed_tokens);
	}

	Head find_head_type(const ParseView view)
	{
		switch (compute::exactly_computable(view)) {
		case compute::Result::complex: return Head{ 0u, Head::Type::complex_computable };
		case compute::Result::natural: return Head{ 0u, Head::Type::natural_computable };
		}

		if (const std::size_t sum_pos = find_first_of_skip_pars(view.tokens, token::sum); sum_pos != TokenView::npos) {
			return Head{ sum_pos, Head::Type::sum };
		}
		if (view.tokens.starts_with(token::unary_minus)) {
			return Head{ 0u, Head::Type::negate };
		}
		if (const std::size_t product_pos = find_first_of_skip_pars(view.tokens, token::product); product_pos != TokenView::npos) {
			return Head{ product_pos, Head::Type::product };
		}
		if (const std::size_t pow_pos = find_first_of_skip_pars(view.tokens, token::hat); pow_pos != TokenView::npos) {
			return Head{ pow_pos, Head::Type::power };
		}
		{
			const std::size_t not_number_pos = view.tokens.find_first_not_of(token::number);
			if (not_number_pos == TokenView::npos) {
				return Head{ 0u, Head::Type::real_value };
			}
			else if (not_number_pos + 1u == view.size() && view.tokens.ends_with(token::imag_unit)) {
				return Head{ 0u, Head::Type::imag_value };
			}
		}
		const std::size_t first_not_character = view.tokens.find_first_not_of(token::character);
		if (first_not_character == TokenView::npos) {
			return Head{ 0u, Head::Type::symbol };
		}
		if (first_not_character == 0u && view.tokens[first_not_character] == token::dollar) {
			if (view.tokens.find_first_not_of(token::dollar) != TokenView::npos) [[unlikely]] throw ParseFailure{ view.offset, "ill formed lambda parameter" };
			return Head{ 0u, Head::Type::parameter };
		}
		if (view.tokens[first_not_character] != token::open_grouping) [[unlikely]] throw ParseFailure{ first_not_character + view.offset, "illegal character, expected '('" };
		if (!view.tokens.ends_with(token::clse_grouping)) [[unlikely]] throw ParseFailure{ view.size() + view.offset, "poor grouping, expected ')'" };
		if (first_not_character == 0u) {
			return Head{ 0u, Head::Type::group };
		}
		else {
			return Head{ first_not_character, Head::Type::function };
		}
	} //find_head_type

	MathIdx build(MathStore& store, ParseView input)
	{
		if (input.size() == 0u) [[unlikely]] throw ParseFailure{ input.offset, "recieved empty substring" };
		Head head = find_head_type(input);
		while (head.type == Head::Type::group) {
			input.remove_prefix(1u);
			input.remove_suffix(1u);
			head = find_head_type(input);
		}
		switch (head.type) {
		case Head::Type::sum: {
			return build_variadic<SumTraits>(store, input, head.where, build_negated<MathStore>, build);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1u);  //remove minus sign
			const MathIdx to_negate = build(store, input);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::product: {
			return build_variadic<ProductTraits>(store, input, head.where, build_inverted<MathStore>, build);
		} break;
		case Head::Type::power: {
			const auto base_view = input.steal_prefix(head.where);
			input.remove_prefix(1u); //remove hat
			const MathIdx base = build(store, base_view);
			const MathIdx expo = build(store, input);
			const std::size_t result_index = store.allocate_one();
			store.at(result_index) = IndexVector{ base, expo };
			return MathIdx(result_index, Fn::pow);
		} break;
		case Head::Type::complex_computable: {
			return build_value(store, compute::eval_complex(input));
		} break;
		case Head::Type::natural_computable: { 
			return build_value(store, compute::eval_natural(input));
		} break;
		case Head::Type::real_value: {
			return build_value(store, compute::parse_value(input));
		} break;
		case Head::Type::imag_value: {
			input.remove_suffix(1u); //remove token::imag_unit
			return build_value(store, Complex(0.0, compute::parse_value(input)));
		} break;
		case Head::Type::function: {
			return build_function(store, input, head.where, build);
		} break;
		case Head::Type::parameter: {
			input.remove_prefix(1u); //remove dollar symbol
			return MathIdx(compute::parse_value(input), LambdaParam{});
		} break;
		case Head::Type::symbol: {
			return MathIdx(CharVector::build(store, input.to_string_view()), Literal::symbol);
		} break;
		default:
			assert(false); 
			return MathIdx();
		}
	} //build




	namespace print {

		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////////////local (print) definitions//////////////////////////////////////////////////////////////////////////
		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

		//operator precedence (used to decide if parentheses are nessecary in out string)
		constexpr int infixr(MathType type)
		{
			constexpr auto infixr_table = std::to_array<std::pair<MathType, int>>({
				{ Comm::sum          , 2000 },
				{ Comm::product      , 2001 },
				{ Fn::pow            , 3000 }, //not between other function types -> assumed to be printed with '^'  
				{ Literal::symbol    , 5000 },
				{ Literal::complex   , 5000 }, //may be printed as sum/product itself, then (maybe) has to add parentheses on its own
				{ LambdaParam{}      , 5000 },
			});
			static_assert(std::is_sorted(infixr_table.begin(), infixr_table.end(), [](auto a, auto b) { return a.second < b.second; }));

			constexpr std::pair<MathType, int> default_infixr = std::make_pair(MathType(0u), 0);
			return search(infixr_table, &std::pair<MathType, int>::first, type, default_infixr).second;
		}

		void append_real(double val, std::string& dest)
		{
			std::stringstream buffer;
			buffer << val;
			dest.append(buffer.str());
		}


		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////////////exported in header/////////////////////////////////////////////////////////////////////////////////
		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


		void append_complex(const std::complex<double> val, std::string& dest, int parent_infixr)
		{
			std::stringstream buffer;

			enum class Flag { showpos, noshowpos };
			const auto add_im_to_stream = [&buffer](const double im, Flag flag) {
				if (im == -1.0) {
					buffer << '-';
				}
				else if (im == 1.0) {
					if (flag == Flag::showpos) {
						buffer << '+';
					}
				}
				else {
					buffer << (flag == Flag::showpos ? std::showpos : std::noshowpos) << im;
				}
				buffer << 'i';
			};

			bool parentheses = false;

			if (val.real() != 0.0 && val.imag() != 0.0) {
				parentheses = parent_infixr > infixr(MathType(Comm::sum));
				buffer << val.real();
				add_im_to_stream(val.imag(), Flag::showpos);
			}
			else if (val.real() != 0.0 && val.imag() == 0.0) {
				parentheses = val.real() < 0.0 && parent_infixr >= infixr(MathType(Comm::sum));	//leading '-'
				buffer << val.real();
			}
			else if (val.real() == 0.0 && val.imag() != 0.0) {
				parentheses = val.imag() < 0.0 && parent_infixr >= infixr(MathType(Comm::sum));	//leading '-'	
				parentheses |= parent_infixr > infixr(MathType(Comm::product));	//*i
				add_im_to_stream(val.imag(), Flag::noshowpos);
			}
			else {
				buffer << '0';
			}

			if (parentheses) {
				dest.push_back('(');
				dest.append(buffer.str());
				dest.push_back(')');
			}
			else {
				dest.append(buffer.str());
			}
		} //append_complex

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr)
		{
			const int own_infixr = infixr(ref.type);
			if (own_infixr <= parent_infixr) {
				str.push_back('(');
			}

			switch (ref.type) {
			case MathType(Comm::sum): {
				const char* seperator = "";
				for (const auto summand : fn::range(ref)) {
					str.append(std::exchange(seperator, "+"));
					print::append_to_string(ref.at(summand), str, own_infixr);
				}
			} break;
			case MathType(Comm::product): {
				const char* seperator = "";
				for (const auto factor : fn::range(ref)) {
					str.append(std::exchange(seperator, "*"));
					print::append_to_string(ref.at(factor), str, own_infixr);
				}
			} break;
			case MathType(Fn::pow): {
				const IndexVector& params = *ref;
				print::append_to_string(ref.at(params[0]), str, own_infixr);
				str.push_back('^');
				print::append_to_string(ref.at(params[1]), str, own_infixr);
			} break;
			default: {
				str.pop_back(); //pop '('
				if (ref.type.is<NamedFn>()) {
					const CharVector& name = fn::named_fn_name(ref);
					str.append(name);
				}
				else if (ref.type.is<Fn>()) {
					str.append(fn::name_of(ref.type.to<Fn>()));
				}
				else {
					assert(ref.type.is<Variadic>());
					str.append(fn::name_of(ref.type.to<Variadic>()));
				}
				str.push_back('(');
				const char* seperator = "";
				for (const auto param : fn::range(ref)) {
					str.append(std::exchange(seperator, ", "));
					print::append_to_string(ref.at(param), str, own_infixr);
				}
			} break;
			case MathType(Literal::symbol): {
				str += ref->characters;
			} break;
			case MathType(Literal::complex): {
				append_complex(ref->complex, str, parent_infixr);
			} break;
			case MathType(LambdaParam{}): {
				str += "$";
				str += std::to_string(ref.index);
			} break;
			}

			if (own_infixr <= parent_infixr) {
				str.push_back(')');
			}
		} //append_to_string (for math)

		std::string to_pretty_string(const UnsaveRef ref, const int parent_infixr)
		{
			std::string str;

			bool need_parentheses = infixr(ref.type) <= parent_infixr;

			const auto get_negative_real = [](const UnsaveRef ref) ->OptionalDouble {
				if (ref.type == Literal::complex) {
					const Complex& complex = *ref;
					if (complex.real() < 0.0 && complex.imag() == 0.0) {
						return { complex.real() };
					}
				}
				return {};
			}; //get_negative_real

			 //returns base, if ref is actually <base>^(-1)
			const auto get_pow_neg1 = [get_negative_real](const UnsaveRef ref) -> std::optional<MathIdx> {
				if (ref.type == Fn::pow) {
					const IndexVector& params = *ref;
					if (const auto expo = get_negative_real(ref.at(params[1]))) {
						if (*expo == -1.0) {
							return { params[0] };
						}
					}
				}
				return {};
			}; //get_pow_neg1

			struct GetNegativeProductResult { double negative_factor; StupidBufferVector<MathIdx, 8> other_factors; };
			const auto get_negative_product = [get_negative_real](const UnsaveRef ref) -> std::optional<GetNegativeProductResult> {
				if (ref.type == Comm::product) {
					StupidBufferVector<MathIdx, 8> other_factors;
					double negative_factor;
					bool found_negative_factor = false;
					for (const auto factor : fn::range(ref)) {
						if (!found_negative_factor) {
							if (const auto negative_val = get_negative_real(ref.at(factor))) {
								negative_factor = *negative_val;
								found_negative_factor = true;
								continue;
							}
						}
						other_factors.push_back(factor);
					}
					if (found_negative_factor) {
						return { { negative_factor, std::move(other_factors)} };
					}
				}
				return {};
			}; //get_negative_product

			const auto append_product = [get_negative_real, get_pow_neg1, &ref, &str](const auto& vec) {
				bool first = true;
				for (const auto elem : vec) {
					if (auto val = get_negative_real(ref.at(elem)); val && first && *val == -1.0) {
						str += "-";
					}
					else if (const auto base = get_pow_neg1(ref.at(elem))) {
						str += (first ? "1/" : "/"); 
						str += print::to_pretty_string(ref.at(*base), infixr(Comm::product));
						first = false;
					}
					else {
						str += (first ? "" : " ");
						//str += (first ? "" : "*");
						str += print::to_pretty_string(ref.at(elem), infixr(Comm::product));
						first = false;
					}
				}
				assert(!first && "found product with only single factor -1 or zero factors");
			}; //append_product

			switch (ref.type) {
			case MathType(Comm::sum): {
				bool first = true;
				for (const auto summand : fn::range(ref)) {
					if (const auto val = get_negative_real(ref.at(summand))) {
						str += (first ? "" : " ");
						append_real(*val, str);
					}
					else if (auto product = get_negative_product(ref.at(summand))) {
						if (product->negative_factor != -1.0) {
							str += (first ? "" : " ");
							append_real(product->negative_factor, str);
							str += " ";
							//str += "*";
						}
						else {
							str +=  (first ? "-" : " - ");
							//str += "-";
						}
						append_product(product->other_factors);
					}
					else {
						str += (first ? "" : " + ");
						//str += (first ? "" : "+");
						str += print::to_pretty_string(ref.at(summand), infixr(ref.type));
					}
					first = false;
				}
				assert(!first && "found sum with zero summands");
			} break;
			case MathType(Comm::product): {
				append_product(fn::range(ref));
			} break;
			case MathType(Fn::pow): {
				const IndexVector& params = *ref;
				if (const OptionalDouble negative_expo = get_negative_real(ref.at(params[1]))) {
					str += "1/";
					str += print::to_pretty_string(ref.at(params[0]), infixr(Fn::pow));
					if (*negative_expo != -1.0) {
						str += "^";
						append_real(-*negative_expo, str);
					}
				}
				else {
					str += print::to_pretty_string(ref.at(params[0]), infixr(ref.type));
					str += "^";
					str += print::to_pretty_string(ref.at(params[1]), infixr(ref.type));
				}
			} break;
			case MathType(NonComm::call): {
				bool first = true;
				const char* seperator = "";
				for (const auto param : fn::range(ref)) {
					if (std::exchange(first, false)) {
						str += print::to_pretty_string(ref.at(param), infixr(ref.type));
						str.push_back('(');
					}
					else {
						str.append(std::exchange(seperator, ", "));
						str += print::to_pretty_string(ref.at(param), infixr(ref.type));
					}
				}
				if (!first) {
					str.push_back(')');
					need_parentheses = false;
					break;
				}
			} [[fallthrough]];
			default: {
				need_parentheses = false;
				if (ref.type.is<NamedFn>()) {
					const CharVector& name = fn::named_fn_name(ref);
					str += std::string_view(name.data(), name.size());
				}
				else if (ref.type.is<Fn>()) {
					str += fn::name_of(ref.type.to<Fn>());
				}
				else {
					assert(ref.type.is<Variadic>());
					str += fn::name_of(ref.type.to<Variadic>());
				}
				str.push_back('(');
				const char* separator = "";
				for (const auto param : fn::range(ref)) {
					str += std::exchange(separator, ", ");
					str += print::to_pretty_string(ref.at(param), infixr(ref.type));
				}
				str.push_back(')');
			} break;
			case MathType(Literal::symbol): {
				str += std::string_view(ref->characters.data(), ref->characters.size());
			} break;
			case MathType(Literal::complex): {
				append_complex(ref->complex, str, parent_infixr);
			} break;
			case MathType(LambdaParam{}): {
				str += "$";
				str += std::to_string(ref.index);
			} break;
			}

			if (need_parentheses) {
				return '(' + str + ')';
			}
			else {
				return str;
			}
		} //to_pretty_string

		void append_memory_row(const Ref ref, std::vector<std::string>& rows)
		{
			const auto show_string_nodes = [&ref, &rows](std::uint32_t idx, bool show_first) {
				const CharVector& var = ref.store->at(idx);
				const std::size_t end = idx + var.node_count();
				if (!show_first) {
					idx++;
				}
				while (idx < end) {
					rows[idx++] += "(CharVector)";
				}
			};
			if (ref.type.is<LambdaParam>()) {
				return;
			}

			std::string& current_str = rows[ref.index];
			switch (ref.type) {
			default: {
				//name:
				if (ref.type.is<NamedFn>()) {
					current_str += fn::named_fn_name(ref);
					show_string_nodes(fn::named_fn_name_index(ref), true);
				}
				else if (ref.type.is<Variadic>()) {
					current_str += fn::name_of(ref.type.to<Variadic>());
				}
				else {
					assert(ref.type.is<Fn>());
					current_str += fn::name_of(ref.type.to<Fn>());
				}

				//parameters:
				current_str.append(std::max(0, 11 - (int)current_str.size()), ' ');
				current_str += ":      { ";
				const char* separator = "";
				const IndexVector& vec = *ref;				
				std::string* current_line = &current_str;
				for (std::size_t vec_idx = 0u; vec_idx < vec.size(); vec_idx++) {
					*current_line += std::exchange(separator, ", ");
					if ((vec_idx - IndexVector::min_capacity) % IndexVector::values_per_node == 0) {
						*(++current_line) += "(Node)         ";
					}
					const std::size_t elem_idx = vec[vec_idx].get_index();
					if (elem_idx < 10) {
						*current_line += ' '; //pleeeaaasee std::format, where are you? :(
					}
					if (elem_idx < 100) {
						*current_line += ' '; //pleeeaaasee std::format, where are you? :(
					}
					*current_line += std::to_string(elem_idx);
					print::append_memory_row(ref.at(vec[vec_idx]), rows);
				}
				*current_line += " }";
			} break;
			case MathType(Literal::symbol): {
				current_str += "symbol     : ";
				show_string_nodes(ref.index, false);
			} break;
			case MathType(Literal::complex): {
				current_str += "value      : ";
			} break;
			}

			//append name of subterm to line
			current_str.append(std::max(0, 38 - (int)current_str.size()), ' ');
			print::append_to_string(ref, current_str, 0);
		} //append_memory_row

		std::string to_memory_layout(const MathStore& store, const std::initializer_list<const MathIdx> heads)
		{
			std::vector<std::string> rows(std::max(store.size(), pattern::match::MatchData::max_variadic_count), "");

			std::string result((heads.size() == 1u) ? "  head at index: " : "  heads at indices: ");
			result.reserve(store.size() * 15);
			{
				const char* separator = "";
				for (const auto head : heads) {
					result += std::exchange(separator, ", ");
					result += std::to_string(head.get_index());
					print::append_memory_row(Ref(store, head), rows);
				}
				result += "\n";
			}	
			{
				const BitVector used_positions = store.storage_occupancy();
				for (int i = 0; i < store.size(); i++) {
					if (i < 10) { //please std::format, i need you :(
						result += "  ";
					}
					else if (i < 100) {
						result += " ";
					}
					result += std::to_string(i);
					result += " | ";
					result += rows[i];
					if (!used_positions.test(i)) {
						result += "-----free slot-----";
					}
					result += "\n";
				}
			}
			return result;
		} //to_memory_layout

		//line name assumes '\0' as last character
		void append_tree_row(const UnsaveRef ref, std::vector<std::string>& rows, const std::size_t offset)
		{
			constexpr std::size_t tab_width = 3u; //specifies how may more characters the subtrees of ref are shifted right, compared to ref

			rows.push_back(std::string(offset, ' '));
			std::string& current_str = rows.back();

			//appending current_str to previously inserted parts of tree visually
			if (rows.size() > 1u) { //(only nessecairy if there are previously inserted parts)
				constexpr signed char bar_up_down = -77;		//(179)	  (these are probably locale dependent, but i dont care)
				constexpr signed char bar_up_right = -64;		//(192)	  (these are probably locale dependent, but i dont care)
				constexpr signed char bar_up_right_down = -61;	//(195)	  (these are probably locale dependent, but i dont care)
				constexpr signed char bar_left_right = -60;	    //(196)	  (these are probably locale dependent, but i dont care)

				//the tab_with many chars (currently) at the end of current_str can be divided into tree parts: 
				//  space(s), the one vertical bar char and horizontal bar chars.
				//so far, all of current_str's last tab_width many characters are spaces, thus the spaces need not to be drawn here.
				constexpr std::size_t horizontal_bar_length = 1u;
				static_assert(tab_width >= horizontal_bar_length + 1u);
				{ //draw vertical bar
					std::size_t row = rows.size() - 2u; //start one row above current_str
					const std::size_t col = offset - 1u - horizontal_bar_length; //(offset - 1u) is pos of last char in current_str
					for (; rows[row][col] == ' '; row--) {
						rows[row][col] = bar_up_down;
					}
					if (rows[row][col] == bar_up_right) {
						rows[row][col] = bar_up_right_down;
					}
				}				
				{ //draw horizontal bar at current hight
					std::size_t col = current_str.size() - 1u;
					for (; col >= offset - horizontal_bar_length; col--) {
						current_str[col] = bar_left_right;
					}
					current_str[col] = bar_up_right;
				}
			}

			switch (ref.type) {
			default: {
				if (ref.type.is<NamedFn>()) {
					const CharVector& name = fn::named_fn_name(ref);
					current_str += std::string_view(name.data(), name.size());
				}
				else if (ref.type.is<Fn>()) {
					current_str += fn::name_of(ref.type.to<Fn>());
				}
				else {
					assert(ref.type.is<Variadic>());
					current_str += fn::name_of(ref.type.to<Variadic>());
				}
				for (const MathIdx param : fn::range(ref)) {
					print::append_tree_row(ref.at(param), rows, offset + tab_width);
				}
			} break;
			case MathType(Literal::symbol): {
				current_str += ' ';
				current_str += std::string_view(ref->characters.data(), ref->characters.size());
			} break;
			case MathType(Literal::complex): {
				current_str += ' ';
				print::append_complex(*ref, current_str, 0u);
			} break;
			case MathType(LambdaParam{}): {
				current_str += " $";
				current_str += std::to_string(ref.index);
			} break;
			}
		} //append_tree_row

		std::string to_tree(const UnsaveRef ref, const std::size_t offset)
		{
			std::vector<std::string> rows;
			print::append_tree_row(ref, rows, offset);

			std::string result;
			for (const auto& row : rows) {
				result.append(row);
				result.push_back('\n');
			}
			return result;
		} //to_tree

	} //namespace print

} //namespace bmath::intern
