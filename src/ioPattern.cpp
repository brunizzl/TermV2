
#include "ioPattern.hpp"
#include "ioArithmetic.hpp"

namespace bmath::intern::pattern {


	PatternParts::PatternParts(const ParseView input)
	{
		const std::size_t bar = find_first_of_skip_pars(input.tokens, token::bar);
		if (bar != TokenView::npos) {
			if (count_skip_pars(input.tokens, token::bar) > 1u) [[unlikely]] throw ParseFailure{ input.offset + bar, "expected only this '|', no further ones at top grouping level" };

			const char illegal_tokens[] = { token::number, token::imag_unit, token::unary_minus, token::sum, token::product, token::hat, token::equals, '\0' };
			const std::size_t illegal_pos = input.tokens.find_first_of(illegal_tokens);
			if (illegal_pos < bar) [[unlikely]] throw ParseFailure{ input.offset + illegal_pos, "unexpected token in declaration" };
		}

		const std::size_t equals = find_first_of_skip_pars(input.tokens, token::equals);
		if (count_skip_pars(input.tokens, token::equals) > 1u) [[unlikely]] throw ParseFailure{ input.offset + equals, "expected only this '=', no further ones at top grouping level" };
		if (equals == TokenView::npos) [[unlikely]] throw ParseFailure{ input.size() - 1u, "expected '=' at top grouping level" };

		if (bar != TokenView::npos) {
			this->declarations = input.substr(0u, bar);
			this->lhs = input.substr(bar + 1u, equals - bar - 1u);
			this->rhs = input.substr(equals + 1u);
		}
		else {
			this->declarations = input.substr(0u, 0u);
			this->lhs = input.substr(0u, equals);
			this->rhs = input.substr(equals + 1u);
		}
	} //PatternParts::PatternParts

	NameLookupTable::NameLookupTable(ParseView declarations)
	{
		const auto parse_declaration = [this](ParseView var_view) {
			const std::size_t colon = find_first_of_skip_pars(var_view.tokens, token::colon);
			if (colon != TokenView::npos) {
				const PnVariablesType type = type_of(var_view.to_string_view(colon + 1u));
				if (type.is<UnknownPnVar>()) [[unlikely]] throw ParseFailure{ var_view.offset + colon + 1u, "unknown restriction" };

				if (type.is<ValueDomain>()) {
					this->value_table.emplace_back(var_view.to_string_view(0, colon), type.to<ValueDomain>());
				}
				else if (type.is<Multi>()) {
					this->multi_table.emplace_back(var_view.to_string_view(0, colon), type.to<Multi>());
				}
				else {
					assert(type.is<TreeMatchOwning>());
					this->tree_table.emplace_back(var_view.to_string_view(0, colon), type.to<TreeMatchOwning>());
				}
			}
			else {
				this->tree_table.emplace_back(var_view.to_string_view(), Restriction::any);
			}
		};

		if (declarations.size()) {
			const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
			parse_declaration(declarations.steal_prefix(comma));
		}
		while (declarations.size()) {
			declarations.remove_prefix(1); //erase comma
			const std::size_t comma = find_first_of_skip_pars(declarations.tokens, token::comma);
			parse_declaration(declarations.steal_prefix(comma));
		}
	} //NameLookupTable::NameLookupTable

	MathIdx NameLookupTable::insert_instance(MathStore& store, const ParseView input)
	{
		const auto name = input.to_string_view();
		const auto search_name = [name](auto& vec) {
			return std::find_if(vec.begin(), vec.end(), [name](const auto& x) { return x.name == name; });
		};

		if (const auto iter = search_name(this->tree_table); iter != this->tree_table.end()) {
			const std::uint32_t match_data_idx = std::distance(this->tree_table.begin(), iter);
			const MathIdx result_typedidx = math_rep::IntermediateTreeMatch::build(store, match_data_idx, iter->restr);
			(this->build_lhs ?
				iter->lhs_instances :
				iter->rhs_instances).push_back(result_typedidx);
			return result_typedidx;
		}
		else if (const auto iter = search_name(this->value_table); iter != this->value_table.end()) {
			const std::uint32_t match_data_idx = std::distance(this->value_table.begin(), iter);
			const MathIdx result_typedidx = math_rep::IntermediateValueMatch::build(store, match_data_idx, iter->domain);
			(this->build_lhs ?
				iter->lhs_instances :
				iter->rhs_instances).push_back(result_typedidx);
			return result_typedidx;
		}
		else if (const auto iter = search_name(this->multi_table); iter != this->multi_table.end()) {
			const std::uint32_t match_data_idx = std::distance(this->multi_table.begin(), iter);
			const MathIdx result_typedidx = math_rep::IntermediateMultiMatch::build(store, match_data_idx, iter->type);
			this->build_lhs ?
				++(iter->lhs_count) :
				++(iter->rhs_count);
			return result_typedidx;
		}
		throw ParseFailure{ input.offset, "match variable has not been declared" };
	} //NameLookupTable::insert_instance

	MathIdx PatternBuildFunction::operator()(MathStore& store, ParseView input)
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
			return build_variadic<SumTraits>(store, input, head.where, build_negated<MathStore>, *this);
		} break;
		case Head::Type::negate: {
			input.remove_prefix(1u);  //remove minus sign
			const MathIdx to_negate = this->operator()(store, input);
			return build_negated(store, to_negate);
		} break;
		case Head::Type::product: {
			return build_variadic<ProductTraits>(store, input, head.where, build_inverted<MathStore>, *this);
		} break;
		case Head::Type::power: {
			const auto base_view = input.steal_prefix(head.where);
			input.remove_prefix(1u); //remove hat
			const MathIdx base = this->operator()(store, base_view);
			const MathIdx expo = this->operator()(store, input);
			const std::size_t result_index = store.allocate_one();
			store.at(result_index) = IndexVector{ base, expo };
			return MathIdx(result_index, MathType(Fn::pow));
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
			return build_function(store, input, head.where, *this);
		} break;
		case Head::Type::parameter: {
			input.remove_prefix(1u); //remove dollar symbol
			return MathIdx(compute::parse_value(input), LambdaParam{});
		} break;
		case Head::Type::symbol: {
			if (input.chars[0u] == '\'') {
				if (input.chars[input.size() - 1u] != '\'') [[unlikely]] throw ParseFailure{ input.offset + 1u, "found no matching \"'\"" };
				return MathIdx(CharVector::build(store, input.to_string_view(1u, input.size() - 1u)), MathType(Literal::symbol));
			}
			else {
				return this->table.insert_instance(store, input);
			}
		} break;
		default:
			assert(false);
			return MathIdx();
		}
	} //PatternBuildFunction::operator()

} //namespace bmath::intern::pattern

namespace bmath::intern::print {

	void append_to_string(const pattern::UnsavePnRef ref, std::string& str, const int depth)
	{
		using namespace pattern;

		const char open_paren = std::array<char, 3>{ '(', '[', '{' } [depth % 3] ;
		const char clse_paren = std::array<char, 3>{ ')', ']', '}' } [depth % 3] ;

		switch (ref.type) {
		default: {
			if (ref.type.is<TreeMatchOwning>()) {
				str.append("_T");
				str.append(std::to_string(ref.index));
				if (ref.type != Restriction::any) {
					str.push_back(open_paren);
					str.append(name_of(ref.type.to<TreeMatchOwning>()));
					str.push_back(clse_paren);
				}
				break;
			}
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
			str.push_back(open_paren);
			const char* seperator = "";
			for (const auto param : fn::range(ref)) {
				str.append(std::exchange(seperator, ", "));
				print::append_to_string(ref.new_at(param), str, depth + 1);
			}
			str.push_back(clse_paren);
		} break;
		case PnType(Literal::symbol): {
			str += ref->characters;
		} break;
		case PnType(Literal::complex): {
			append_complex(ref->complex, str, 0);
		} break;
		case PnType(TreeMatchNonOwning{}): {
			str.append("_T");
			str.append(std::to_string(ref.index));
			str.push_back('\'');
		} break;
		case PnType(LambdaParam{}): {
			str.append("$");
			str.append(std::to_string(ref.index));
		} break;
		case PnType(ValueMatch::non_owning):
			[[fallthrough]];
		case PnType(ValueMatch::owning): {
			const ValueMatchVariable& var = *ref;
			str.append("_V");
			str.append(std::to_string(var.match_data_idx));
			if (ref.type == ValueMatch::non_owning) {
				str.push_back('\'');
			}
			str.push_back(open_paren);
			str.append(name_of(var.domain));
			str.append(", ");
			print::append_to_string(ref.new_at(var.mtch_idx), str, depth + 1);
			str.push_back(clse_paren);
		} break;
		case PnType(ValueProxy{}): {
			str.append("_VP");
			str.append(std::to_string(ref.index));
		} break;
		case PnType(MultiMatch::fst): 
		case PnType(MultiMatch::snd): {
			str.append("_P");
			str.append(std::to_string(ref.index));
			str.append("...");
		} break;
		}
	} //append_to_string (for pattern)

	void appent_to_simple_tree(const pattern::UnsavePnRef ref, std::string& str, const int depth)
	{
		using namespace pattern;

		str.push_back('\n');
		str.append(depth * 4ull, ' ');

		switch (ref.type) {
		default: {
			if (ref.type.is<TreeMatchOwning>()) {
				str.append("_T");
				str.append(std::to_string(ref.index));
				if (ref.type != Restriction::any) {
					str.push_back('(');
					str.append(name_of(ref.type.to<TreeMatchOwning>()));
					str.push_back(')');
				}
				break;
			}
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
			str.push_back(':');
			for (const auto param : fn::range(ref)) {
				print::appent_to_simple_tree(ref.new_at(param), str, depth + 1);
			}
		} break;
		case PnType(Literal::symbol): {
			str += ref->characters;
		} break;
		case PnType(Literal::complex): {
			append_complex(ref->complex, str, 0);
		} break;
		case PnType(TreeMatchNonOwning{}): {
			str.append("_T");
			str.append(std::to_string(ref.index));
			str.push_back('\'');
		} break;
		case PnType(LambdaParam{}): {
			str.append("$");
			str.append(std::to_string(ref.index));
		} break;
		case PnType(ValueMatch::non_owning):
			[[fallthrough]];
		case PnType(ValueMatch::owning): {
			const ValueMatchVariable& var = *ref;
			str.append("_V");
			str.append(std::to_string(var.match_data_idx));
			if (ref.type == ValueMatch::non_owning) {
				str.push_back('\'');
			}
			str.push_back('(');
			str.append(name_of(var.domain));
			str.append(", ");
			print::append_to_string(ref.new_at(var.mtch_idx), str, depth + 1);
			str.push_back(')');
		} break;
		case PnType(ValueProxy{}): {
			str.append("_VP");
			str.append(std::to_string(ref.index));
		} break;
		case PnType(MultiMatch::fst): 
		case PnType(MultiMatch::snd): {
			str.append("_P");
			str.append(std::to_string(ref.index));
			str.append("...");
		} break;
		}
	} //append_to_string (for pattern)

} //namespace bmath::intern::print
