
#include <iostream>
#include <cfenv>

#include "pattern.hpp"
#include "ioArithmetic.hpp"
#include "fold.hpp"


namespace bmath::intern::pattern {

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////local definitions//////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	//all patterns are initially build using only MathType, thus the pattern specific nodes 
	//  are modeled with ones avaliable in math (most prominently: NamedFn)
	namespace math_rep {

		//TreeMatchVariable is modeled as NamedFn holding:
		//  .match_data_idx as complex in first parameter
		//  .restr as variable (same name as calling name_of(.restr)) in second parameter
		class IntermediateTreeMatch
		{
			UnsaveRef ref;

			constexpr IntermediateTreeMatch(const UnsaveRef new_ref) noexcept :ref(new_ref) {}

		public:
			static constexpr std::string_view function_name = "_TM";

			template<StoreLike S>
			static constexpr MathIdx build(S& store, const std::uint32_t match_data_idx, const Restriction restr)
			{
				const std::array<MathIdx, 2> parameters = {
					build_value(store, Complex{ static_cast<double>(match_data_idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(restr)), MathType(Literal::variable)),
				};
				return fn::build_named_fn<MathType>(store, function_name, parameters);
			}

			static constexpr std::optional<IntermediateTreeMatch> cast(const UnsaveRef new_ref)
			{
				if (new_ref.type == NamedFn{}) {
					std::string_view ref_name = fn::named_fn_name(new_ref);
					if (ref_name == function_name) {
						const IndexVector& params = *new_ref;
						assert(params.size() == 2);
						assert(params[0].get_type() == Literal::complex);
						assert(has_form(*new_ref.new_at(params[0]), Form::natural_0));
						assert(params[1].get_type() == Literal::variable);
						assert(type_of(new_ref.new_at(params[1])->char_vec).is<Restriction>());
						return IntermediateTreeMatch(new_ref);
					}
				}
				return std::nullopt;
			}

			constexpr std::uint32_t match_data_idx() const noexcept
			{
				const IndexVector& params = *this->ref;
				return this->ref.new_at(params[0])->complex.real();
			}

			constexpr Restriction restr() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[1])->char_vec).to<Restriction>();
			}
		}; //class IntermediateTreeMatch

		//(nonexisting) MultiMatchVariable is modeled as NamedFn holding:
		//  .index() as complex in first parameter
		//  .type() as variable (same name as calling name_of(.get_type())) in second parameter
		class IntermediateMultiMatch
		{
			UnsaveRef ref;

			constexpr IntermediateMultiMatch(const UnsaveRef new_ref) noexcept :ref(new_ref) {}

		public:
			static constexpr std::string_view function_name = "_MM";

			template<StoreLike S>
			static constexpr MathIdx build(S& store, const std::uint32_t idx, const MultiPn type)
			{
				const std::array<MathIdx, 2> parameters = {
					build_value(store, Complex{ static_cast<double>(idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(type)), MathType(Literal::variable)),
				};
				return fn::build_named_fn<MathType>(store, function_name, parameters);
			}

			static constexpr std::optional<IntermediateMultiMatch> cast(const UnsaveRef new_ref)
			{
				if (new_ref.type == NamedFn{}) {
					std::string_view ref_name = fn::named_fn_name(new_ref);
					if (ref_name == function_name) {
						const IndexVector& params = *new_ref;
						assert(params.size() == 2);
						assert(params[0].get_type() == Literal::complex);
						assert(has_form(*new_ref.new_at(params[0]), Form::natural_0));
						assert(params[1].get_type() == Literal::variable);
						assert(type_of(new_ref.new_at(params[1])->char_vec).is<MultiPn>());
						return IntermediateMultiMatch(new_ref);
					}
				}
				return std::nullopt;
			}

			constexpr std::uint32_t index() const noexcept
			{
				const IndexVector& params = *this->ref;
				return this->ref.new_at(params[0])->complex.real();
			}

			constexpr MultiPn type() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[1])->char_vec).to<MultiPn>();
			}
		}; //class IntermediateMultiMatch


		//(nonexisting) ValueProxy is modeled as NamedFn holding:
		//  .index() as complex in first parameter
		class IntermediateValueProxy
		{
			UnsaveRef ref;

			constexpr IntermediateValueProxy(const UnsaveRef new_ref) noexcept :ref(new_ref) {}

		public:
			static constexpr std::string_view function_name = "_VP";

			template<StoreLike S>
			static constexpr MathIdx build(S& store, const std::uint32_t index)
			{
				const std::array<MathIdx, 1> parameters = {
					build_value(store, Complex{ static_cast<double>(index), 0.0 }),
				};
				return fn::build_named_fn<MathType>(store, function_name, parameters);
			}

			static constexpr std::optional<IntermediateValueProxy> cast(const UnsaveRef new_ref)
			{
				if (new_ref.type == NamedFn{}) {
					std::string_view ref_name = fn::named_fn_name(new_ref);
					if (ref_name == function_name) {
						const IndexVector& params = *new_ref;
						assert(params.size() == 1);
						assert(params[0].get_type() == Literal::complex);
						assert(has_form(*new_ref.new_at(params[0]), Form::natural_0));
						return IntermediateValueProxy(new_ref);
					}
				}
				return std::nullopt;
			}

			constexpr std::uint32_t index() const noexcept
			{
				const IndexVector& params = *this->ref;
				return this->ref.new_at(params[0])->complex.real();
			}
		}; //class IntermediateValueProxy

		//ValueMatchVariable is modeled as NamedFn holding:
		//  .mtch_idx in first parameter
		//  .match_data_idx as complex in second parameter
		//  .form as variable (same name as calling name_of(.restr)) in third parameter
		class IntermediateValueMatch
		{
			UnsaveRef ref;

			constexpr IntermediateValueMatch(const UnsaveRef new_ref) noexcept :ref(new_ref) {}

		public:
			static constexpr std::string_view function_name = "_VM";

			template<StoreLike S>
			static constexpr MathIdx build(S& store, const std::uint32_t match_data_idx, const Form form)
			{
				const std::array<MathIdx, 3> parameters = {
					IntermediateValueProxy::build(store, match_data_idx),
					build_value(store, Complex{ static_cast<double>(match_data_idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(form)), MathType(Literal::variable)),
				};
				return fn::build_named_fn<MathType>(store, function_name, parameters);
			}

			static constexpr std::optional<IntermediateValueMatch> cast(const UnsaveRef new_ref)
			{
				if (new_ref.type == NamedFn{}) {
					std::string_view ref_name = fn::named_fn_name(new_ref);
					if (ref_name == function_name) {
						const IndexVector& params = *new_ref;
						assert(params.size() == 3);
						assert(params[1].get_type() == Literal::complex);
						assert(has_form(*new_ref.new_at(params[1]), Form::natural_0));
						assert(params[2].get_type() == Literal::variable);
						assert(type_of(new_ref.new_at(params[2])->char_vec).is<Form>());
						return IntermediateValueMatch(new_ref);
					}
				}
				return std::nullopt;
			}

			constexpr MathIdx mtch_idx() const noexcept { return this->ref->parameters[0]; }

			constexpr std::uint32_t match_data_idx() const noexcept
			{
				const IndexVector& params = *this->ref;
				return this->ref.new_at(params[1])->complex.real();
			}

			constexpr Form form() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[2])->char_vec).to<Form>();
			}
		}; //class IntermediateValueMatch
	} //namespace math_rep

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////  parsing  ////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

				if (type.is<Form>()) {
					this->value_table.emplace_back(var_view.to_string_view(0, colon), type.to<Form>());
				}
				else if (type.is<MultiPn>()) {
					this->multi_table.emplace_back(var_view.to_string_view(0, colon), type.to<MultiPn>());
				}
				else {
					assert(type.is<Restriction>());
					this->tree_table.emplace_back(var_view.to_string_view(0, colon), type.to<Restriction>());
				}
			}
			else {
				this->tree_table.emplace_back(var_view.to_string_view(), Restriction(Restr::any));
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
			const MathIdx result_typedidx = math_rep::IntermediateValueMatch::build(store, match_data_idx, iter->form);
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
		case Head::Type::variable: {
			if (input.chars[0u] == '\'') {
				if (input.chars[input.size() - 1u] != '\'') [[unlikely]] throw ParseFailure{ input.offset + 1u, "found no matching \"'\"" };
				return MathIdx(CharVector::build(store, input.to_string_view(1u, input.size() - 1u)), MathType(Literal::variable));
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


	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////  tree manipulation  //////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	bool meets_restriction(const UnsaveRef ref, const Restriction restr)
	{
		switch (restr) {
		case Restriction(Restr::any):
			return true;
		case Restriction(Restr::no_val):
			return ref.type != Literal::complex;
		case Restriction(Restr::nn1):
			return (ref.type != Literal::complex) || (ref->complex != -1.0);
		case Restriction(Restr::function):
			return ref.type.is<Variadic>() || ref.type.is<Fn>();
		default:
			assert(restr.is<MathType>());
			return restr == ref.type;
		}
	} //meets_restriction

	bool has_form(const Complex& nr, const Form form)
	{
		constexpr double max_save_int = 9007199254740991; //== 2^53 - 1, largest integer explicitly stored in double

		const double re = nr.real();
		const double im = nr.imag();

		bool accept = true;
		switch (form) {
		case Form::natural:   accept &= re > 0.0; [[fallthrough]];
		case Form::natural_0: accept &= re >= 0.0; [[fallthrough]];
		case Form::integer:   accept &= re - std::int64_t(re) == 0.0;
			accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
		case Form::real:      accept &= im == 0.0; [[fallthrough]];
		case Form::complex:
			return accept;

		case Form::negative:      return re < 0.0 && im == 0.0;
		case Form::positive:      return re > 0.0 && im == 0.0;
		case Form::not_negative:  return re >= 0.0 && im == 0.0;
		case Form::not_positive:  return re <= 0.0 && im == 0.0;
		default:
			assert(false);
			return false;
		}
	} //has_form


	IntermediateRewriteRule::IntermediateRewriteRule(std::string name, Convert convert)
	{
		//parsing and such
		auto parse_string = ParseString(name);
		parse_string.allow_implicit_product();
		parse_string.remove_space();
		const auto parts = PatternParts(parse_string);
		auto match_variables_table = NameLookupTable(parts.declarations);
		throw_if(match_variables_table.tree_table.size() > match::MatchData::max_tree_match_count, "too many tree_match match variables declared");
		throw_if(match_variables_table.value_table.size() > match::MatchData::max_value_match_count, "too many value match variables declared");
		PatternBuildFunction build_function = { match_variables_table };
		this->lhs_head = build_function(this->store, parts.lhs);
		match_variables_table.build_lhs = false;
		this->rhs_head = build_function(this->store, parts.rhs);

		if (convert == Convert::all) {	
			static_assert("_VM" == math_rep::IntermediateValueMatch::function_name);
			static_assert("_VP" == math_rep::IntermediateValueProxy::function_name);
			static const auto bubble_up_value_match = std::to_array<RewriteRule>({
				//mtch_idx gets two versions, the second one keeps the original ValueProxy (_VP) with the right match_data_idx (i)
				{ "             k, f |      _VM(_VP(k), k, f)          = _Temp(_VP(k), _VP(k) , k, f)", Convert::basic },

				//computable computations surrounding ValueMatch are "sucked in" to second parameter of (_Temp representation of) ValueMatch
				{ "a :value, m, k, f |      _Temp(_VP(k), m, k, f) + a = _Temp(_VP(k), m + a  , k, f)", Convert::basic },
				{ "a :value, m, k, f |      _Temp(_VP(k), m, k, f) * a = _Temp(_VP(k), m * a  , k, f)", Convert::basic },
				{ "          m, k, f |      _Temp(_VP(k), m, k, f) ^ 2 = _Temp(_VP(k), m ^ 2  , k, f)", Convert::basic },
				{ "          m, k, f | sqrt(_Temp(_VP(k), m, k, f))    = _Temp(_VP(k), sqrt(m), k, f)", Convert::basic },
			});
			this->lhs_head = match::apply_rule_range(bubble_up_value_match.begin(), bubble_up_value_match.end(), this->lhs_mut_ref());

			//the matching of a ValueMatchVariable works by applying the inverse of the operation written down in parsed string to the value to match.
			//this inverse is build here by unwrapping the second parameter and applying the inverse of the unwrapped operations to the first parameter.
			//(the result is found at ValueMatchVariable::mtch_idx)
			static const auto invert_match_subtree = std::to_array<RewriteRule>({
				{ "val :value, m1, m2, k, f | _Temp(m1, m2 + val, k, f) = _Temp(m1 - val, m2, k, f)", Convert::basic },
				{ "val :value, m1, m2, k, f | _Temp(m1, m2 * val, k, f) = _Temp(m1 / val, m2, k, f)", Convert::basic },
				{ "            m1, m2, k, f | _Temp(m1, m2 ^ 2  , k, f) = _Temp(sqrt(m1), m2, k, f)", Convert::basic },
				{ "            m1, m2, k, f | _Temp(m1, sqrt(m2), k, f) = _Temp(m1 ^ 2  , m2, k, f)", Convert::basic },
				//applied once m2 is fully unwrapped: converting back
				{ "            m1,     k, f | _Temp(m1, _VP(k)  , k, f) = _VM(m1, k, f)",             Convert::basic },
			});
			this->lhs_head = match::apply_rule_range(invert_match_subtree.begin(), invert_match_subtree.end(), this->lhs_mut_ref());
			//note it is sometimes desired to use a ValueMatchVariable in a context not allowing it to swallow any of its surroundings. 
			//combining both rulesets would therefore result in an infinite loop in that case, as the very first and very last pattern convert between each other.
		}

		for (const auto& multi_match : match_variables_table.multi_table) {
			throw_if(multi_match.lhs_count > 1u, "pattern only allows single use of each MultiPn in lhs.");
		}

		//sorting and combining is done after rearanging value match to allow constructs 
		//  like "a :real, b | (a+2)+b = ..." to take summands / factors into their value_match match part
		this->lhs_head = tree::establish_basic_order(this->lhs_mut_ref());
		this->rhs_head = tree::establish_basic_order(this->rhs_mut_ref());

		{ //add implicit MultiPn::summands / MultiPn::factors if outermost type of lhs is sum / product
			if (this->lhs_head.get_type() == Comm::sum || this->lhs_head.get_type() == Comm::product) {
				const MathType head_type = this->lhs_head.get_type();
				const IndexVector& head_variadic = this->store.at(this->lhs_head.get_index());
				if (!math_rep::IntermediateMultiMatch::cast(Ref(this->store, head_variadic.back()))) {
					{ //adjust lhs
						const MathIdx new_multi = math_rep::IntermediateMultiMatch::build(this->store, match_variables_table.multi_table.size(),
							head_type == Comm::sum ? MultiPn::summands : MultiPn::factors);
						const std::size_t new_lhs_head_idx = this->store.allocate_one();
						this->store.at(new_lhs_head_idx) = IndexVector({ this->lhs_head, new_multi });
						this->lhs_head = MathIdx(new_lhs_head_idx, head_type);
						this->lhs_head = tree::establish_basic_order(this->lhs_mut_ref());
					}
					{ //adjust rhs
						const MathIdx new_multi = math_rep::IntermediateMultiMatch::build(this->store, match_variables_table.multi_table.size(),
							head_type == Comm::sum ? MultiPn::summands : MultiPn::factors);
						const std::size_t new_rhs_head_idx = this->store.allocate_one();
						this->store.at(new_rhs_head_idx) = IndexVector({ this->rhs_head, new_multi });
						this->rhs_head = MathIdx(new_rhs_head_idx, head_type);
						this->rhs_head = tree::establish_basic_order(this->rhs_mut_ref());
					}
				}
			}
		}			
	} //RewriteRule::RewriteRule

	std::string IntermediateRewriteRule::to_string() const
	{
		std::string str;
		print::append_to_string(this->lhs_ref(), str);
		str.append(" = ");
		print::append_to_string(this->rhs_ref(), str);
		return str;
	}

	std::string IntermediateRewriteRule::to_memory_layout() const
	{
		return print::to_memory_layout(this->store, { this->lhs_head, this->rhs_head });
	}

	std::string IntermediateRewriteRule::lhs_tree(const std::size_t offset) const
	{
		return print::to_tree(this->lhs_ref(), offset);
	}

	std::string IntermediateRewriteRule::rhs_tree(const std::size_t offset) const
	{
		return print::to_tree(this->rhs_ref(), offset);
	}



	RewriteRule::RewriteRule(std::string name, Convert convert)
	{
		{
			IntermediateRewriteRule intermediate = IntermediateRewriteRule(std::move(name), convert);
			assert(tree::valid_storage(intermediate.store, { intermediate.lhs_head, intermediate.rhs_head }));

			this->store.reserve(intermediate.store.nr_used_slots());
			this->lhs_head = pn_tree::intermediate_to_pattern(intermediate.lhs_ref(), this->store, Side::lhs, convert);
			this->rhs_head = pn_tree::intermediate_to_pattern(intermediate.rhs_ref(), this->store, Side::rhs, convert);
			pn_tree::sort(this->lhs_mut_ref());
			pn_tree::sort(this->rhs_mut_ref());
		}
		{ //adjusting MultiPn indices to SharedVariadicDatum index of each MulitPn on lhs (required to be done bevore changing MultiPn types!)

			//has to be filled in same order as MatchData::variadic_data
			//index of element in old_multis equals value of corrected MultiPn occurence (plus the type)
			std::vector<PnIdx> old_multis;
			const auto catalog_lhs_occurences = [&old_multis](const PnRef head) {
				struct Acc
				{
					std::vector<PnIdx>* old_multis;
					std::uint32_t own_idx;

					constexpr Acc(const PnRef ref, std::vector<PnIdx>* new_old_multis) noexcept
						:old_multis(new_old_multis), own_idx(-1u)
					{
						if (ref.type.is<Variadic>()) { //these may contain MultiPn -> these have SharedVariadicDatum entry 
							this->own_idx = this->old_multis->size(); //new last element 
							this->old_multis->emplace_back(PnIdx{}); //becomes only valid element if consume finds MultiPn
						}
					}

					void consume(const PnIdx child) noexcept
					{
						if (child.get_type().is<MultiPn>()) {
							this->old_multis->at(this->own_idx) = child;
						}
					}

					auto result() noexcept { return PnIdx{}; } //caution: only works as long as no multi is represented as Function
				};
				(void)fold::tree_fold<PnIdx, Acc>(head, [](const PnRef ref) { return ref.typed_idx(); }, &old_multis);
			};
			catalog_lhs_occurences(this->lhs_ref());

			const auto replace_occurences = [&old_multis](const MutPnRef head, const bool test_lhs) -> bool {
				const auto check_function_params = [&old_multis, test_lhs](const MutPnRef ref) -> fold::FindTrue {
					if (ref.type.is<Function>()) {
						for (auto& param : fn::unsave_range(ref)) {
							if (param.get_type().is<MultiPn>()) {
								if (!ref.type.is<Variadic>() && test_lhs) { //only Variadic may carry MultiPn in lhs
									return true;
								}
								const auto new_param_pos = std::find(old_multis.begin(), old_multis.end(), param); //relative to begin() to be precise
								if (new_param_pos == old_multis.end()) { //not found -> not present in lhs -> illegal!
									assert(!test_lhs); //we just made old_multis from lhs, thus that should be correct
									return true;
								}
								const std::uint32_t new_param_idx = std::distance(old_multis.begin(), new_param_pos);
								param = PnIdx(new_param_idx, param.get_type());
							}
						}
					}
					return false;
				};
				return fold::simple_fold<fold::FindTrue>(head, check_function_params);
			};
			throw_if(replace_occurences(this->lhs_mut_ref(), true), "MultiPn in unexpected place in lhs");
			throw_if(replace_occurences(this->rhs_mut_ref(), false), "MultiPn only referenced in rhs");
		}
		{
			//if MultiPn::params occurs in sum / product, it is replaced by legal and matching MultiPn version.
			const auto test_and_replace_multi_pn = [](const MutPnRef head, const bool test_lhs) -> bool {
				const auto inspect_variadic = [test_lhs](const MutPnRef ref) -> fold::FindTrue {
					if (ref.type == Comm::sum || ref.type == Comm::product) {
						const PnType representing_type = ref.type == Comm::sum ? MultiPn::summands : MultiPn::factors;
						for (PnIdx& elem : fn::unsave_range(ref)) {
							const PnType elem_type = elem.get_type();
							if (elem_type == representing_type) {
								elem = PnIdx(elem.get_index(), MultiPn::params); //params also represent summands / factors
							}
							else if (test_lhs && elem_type.is<MultiPn>()) {
								//in lhs a sum may never hold factors directly and vice versa
								return true;
							}
						}
					}
					return false;
				};
				return fold::simple_fold<fold::FindTrue>(head, inspect_variadic);
			};
			throw_if(test_and_replace_multi_pn(this->lhs_mut_ref(), true), "wrong MultiPn in lhs");
			test_and_replace_multi_pn(this->rhs_mut_ref(), false);
		}
		{
			const auto contains_illegal_value_match = [](const PnRef head) -> bool {
				const auto inspect_variadic = [](const PnRef ref) -> fold::FindTrue {
					if (ref.type == Comm::sum || ref.type == Comm::product) {
						std::size_t nr_value_matches = 0u;
						for (const PnIdx elem : fn::range(ref)) {
							nr_value_matches += (elem.get_type() == PnNode::value_match);
						}
						return nr_value_matches > 1u;
					}
					return false;
				};
				return fold::simple_fold<fold::FindTrue>(head, inspect_variadic);
			};
			throw_if(contains_illegal_value_match(this->lhs_ref()), "no two value match variables may share the same sum / product in lhs.");
		}
		{
			const auto contains_illegal_multi_match = [](const PnRef head) -> bool {
				const auto inspect_variadic = [](const PnRef ref) -> fold::FindTrue {
					if (ref.type == Comm::sum || ref.type == Comm::product) {
						std::size_t nr_multi_matches = 0u;
						for (const PnIdx elem : fn::range(ref)) {
							nr_multi_matches += elem.get_type().is<MultiPn>();
						}
						return nr_multi_matches > 1u;
					}
					return false;
				};
				return fold::simple_fold<fold::FindTrue>(head, inspect_variadic);
			};
			throw_if(contains_illegal_multi_match(this->lhs_ref()), "no two multi match variables may share the same sum / product in lhs.");
		}
		{
			const auto contains_to_long_variadic = [](const PnRef head) -> bool {
				const auto inspect_variadic = [](const PnRef ref) -> fold::FindTrue {
					if (ref.type == Comm::sum || ref.type == Comm::product) {
						const std::uint32_t multi_at_back = ref->parameters.back().get_type().is<MultiPn>();
						return ref->parameters.size() - multi_at_back > match::SharedVariadicDatum::max_pn_variadic_params_count;
					}
					return false;
				};
				return fold::simple_fold<fold::FindTrue>(head, inspect_variadic);
			};
			throw_if(contains_to_long_variadic(this->lhs_ref()), "a sum / product in lhs contains to many operands.");
		}
		{
			const auto contains_to_many_variadics = [](const PnRef head) -> bool {
				struct Acc
				{
					int acc;

					constexpr Acc(const PnRef ref) noexcept
						:acc(ref.type == PnNode::value_match ?
							std::numeric_limits<int>::min() : //subterms of value_match dont count -> initialize negative
							(ref.type.is<Variadic>() || ref.type.is<NamedFn>())) //count only these two
					{}

					void consume(const int child_size) noexcept { this->acc += child_size; }
					auto result() noexcept { return std::max(this->acc, 0); } //always this->acc if type was other than value_match
				};
				const int variadic_count = fold::tree_fold<std::size_t, Acc>(head, [](auto) { return 0; });
				return variadic_count > match::MatchData::max_variadic_count;
			};
			throw_if(contains_to_many_variadics(this->lhs_ref()), "lhs contains to many variadic functions / instances of NamedFn");
		}
	} //RewriteRule::RewriteRule

	std::string RewriteRule::to_string() const
	{
		std::string str;
		print::append_to_string(this->lhs_ref(), str);
		str.append(" = ");
		print::append_to_string(this->rhs_ref(), str);
		return str;
	} //RewriteRule::to_string

	namespace pn_tree {

		OptionalComplex eval_value_match(const UnsavePnRef ref, const Complex& start_val)
		{
			const auto get_divisor = [](const UnsavePnRef ref) -> std::optional<PnIdx> {
				if (ref.type == Fn::pow) {
					const PnIdxVector& params = *ref;
					if (params[1].get_type() == Literal::complex) {
						if (ref.new_at(params[1])->complex == -1.0) {
							return { params[0] }; //return just base
						}
					}
				}
				return {}; //return nothing
			};

			const auto compute_exact = [](auto operate) -> OptionalComplex {
				std::feclearexcept(FE_ALL_EXCEPT);
				const OptionalComplex result = operate();
				return (!std::fetestexcept(FE_ALL_EXCEPT)) ? result : OptionalComplex();
			};

			switch (ref.type) {
			case PnType(Comm::sum): {
				OptionalComplex result_val = 0.0;
				for (auto& summand : fn::range(ref)) {
					if (const OptionalComplex summand_val = eval_value_match(ref.new_at(summand), start_val)) {
						if (const OptionalComplex res = compute_exact([&] {return result_val + summand_val; })) {
							result_val = res;
							continue;
						}
					}
					return {};
				}
				return result_val;
			} break;
			case PnType(Comm::product): {
				OptionalComplex result_factor = 1.0;
				OptionalComplex result_divisor = 1.0;
				for (auto& factor : fn::range(ref)) {
					if (const std::optional<PnIdx> divisor = get_divisor(ref.new_at(factor))) {
						if (const OptionalComplex divisor_val = eval_value_match(ref.new_at(*divisor), start_val)) {
							if (const OptionalComplex res = compute_exact([&] { return result_divisor * divisor_val; })) {
								result_divisor = res;
								continue;
							}
						}
					}
					if (const OptionalComplex factor_val = eval_value_match(ref.new_at(factor), start_val)) {
						if (const OptionalComplex res = compute_exact([&] {return result_factor * factor_val; })) {
							result_factor = res;
							continue;
						}
					}
					return {};
				}
				return compute_exact([&] { return result_factor / result_divisor; });
			} break;
			default: {
				assert(ref.type.is<Fn>());
				if (const std::optional<PnIdx> divisor = get_divisor(ref)) {
					if (const OptionalComplex divisor_val = eval_value_match(ref.new_at(*divisor), start_val)) {
						return compute_exact([&] { return 1.0 / *divisor_val; });
					}
					else {
						return {};
					}
				}
				const PnIdxVector& params = *ref;
				std::array<OptionalComplex, 4> res_vals;
				for (std::size_t i = 0; i < fn::arity(ref.type.to<Function>()); i++) {
					res_vals[i] = eval_value_match(ref.new_at(params[i]), start_val);
					if (!res_vals[i]) {
						return {};
					}
				}
				return compute_exact([&] { return fn::eval(ref.type.to<Fn>(), res_vals); });
			} break;
			case PnType(Literal::complex):
				return ref->complex;
			case PnType(PnNode::value_proxy):
				return start_val;
			}
		} //eval_value_match

		PnIdx intermediate_to_pattern(const UnsaveRef src_ref, PnStore& dst_store, const Side side, const Convert convert)
		{
			switch (src_ref.type) {
			case MathType(NamedFn{}): {
				const std::string_view name = fn::named_fn_name(src_ref);
				if (const auto tree_match = math_rep::IntermediateTreeMatch::cast(src_ref)) {
					const std::size_t dst_index = dst_store.allocate_one();
					dst_store.at(dst_index) = TreeMatchVariable{ tree_match->match_data_idx(), tree_match->restr() };
					return PnIdx(dst_index, PnNode::tree_match);
				}
				if (const auto multi_match = math_rep::IntermediateMultiMatch::cast(src_ref)) {
					return PnIdx(multi_match->index(), multi_match->type());
				}

				if (convert == Convert::all) {
					if (const auto value_match = math_rep::IntermediateValueMatch::cast(src_ref)) {
						if (side == Side::lhs) {
							const PnIdx mtch_idx = intermediate_to_pattern(src_ref.new_at(value_match->mtch_idx()), dst_store, side, convert);
							const std::uint32_t match_data_idx = value_match->match_data_idx();
							const Form form = value_match->form();

							const std::size_t dst_index = dst_store.allocate_one();
							dst_store.at(dst_index) = ValueMatchVariable(mtch_idx, match_data_idx, form);
							return PnIdx(dst_index, PnNode::value_match);
						}
						else {
							assert(side == Side::rhs);

							const auto value_proxy = math_rep::IntermediateValueProxy::cast(src_ref.new_at(value_match->mtch_idx()));
							assert(value_proxy);
							return PnIdx(value_proxy->index(), PnNode::value_proxy);
						}
					}
					if (const auto value_proxy = math_rep::IntermediateValueProxy::cast(src_ref)) {
						assert(side == Side::lhs);
						return PnIdx(value_proxy->index(), PnNode::value_proxy);
					}
				}
			} [[fallthrough]]; //if NamedFn did not represent a match variable just keep it as is
			default: {
				assert(src_ref.type.is<Function>());
				StupidBufferVector<PnIdx, 12> dst_parameters;
				for (const MathIdx src_param : fn::range(src_ref)) {
					const PnIdx dst_param = intermediate_to_pattern(src_ref.new_at(src_param), dst_store, side, convert);
					dst_parameters.push_back(dst_param);
				}
				if (src_ref.type.is<NamedFn>()) {
					const CharVector& name_ref = fn::named_fn_name(src_ref);
					return fn::build_named_fn<PnType>(dst_store, name_ref, dst_parameters);
				}
				else {
					return PnIdx(PnIdxVector::build(dst_store, dst_parameters), src_ref.type);
				}
			} break;
			case MathType(Literal::variable): {
				const CharVector& src_var = *src_ref;
				const std::size_t dst_index = CharVector::build(dst_store, src_var);
				return PnIdx(dst_index, src_ref.type);
			} break;
			case MathType(Literal::complex): {
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = src_ref->complex; //bitwise copy of src
				return PnIdx(dst_index, src_ref.type);
			} break;
			}
			assert(false);
			return PnIdx();
		} //intermediate_to_pattern

		std::strong_ordering pn_tree::compare(const UnsavePnRef fst, const UnsavePnRef snd)
		{
			const auto compare_char_vecs = [](const CharVector& fst, const CharVector& snd) -> std::strong_ordering {
				const std::size_t size = std::min(fst.size(), snd.size());
				if (const std::strong_ordering cmp = compare_arrays(fst.data(), snd.data(), size);
					cmp != std::strong_ordering::equal)
				{
					return cmp;
				}
				return fst.size() <=> snd.size();
			};

			if (fst.type != snd.type) {
				return generality(fst.type) <=> generality(snd.type);
			}
			
			switch (fst.type) {
			case PnType(NamedFn{}): {
				const CharVector& name_1 = fn::named_fn_name(fst);
				const CharVector& name_2 = fn::named_fn_name(snd);
				if (const std::strong_ordering cmp = compare_char_vecs(name_1, name_2); 
					cmp != std::strong_ordering::equal) {
					return cmp;
				}
			} [[fallthrough]];
			default: {
				assert(fst.type.is<Function>());
				const PnIdxVector& fst_vec = *fst;
				const PnIdxVector& snd_vec = *snd;

				auto iter_1 = fst_vec.begin();
				auto iter_2 = snd_vec.begin();
				const auto stop_1 = fst_vec.end();
				const auto stop_2 = snd_vec.end();
				for (; iter_1 != stop_1 && iter_2 != stop_2; ++iter_1, ++iter_2) {
					const auto iter_cmp = pn_tree::compare(fst.new_at(*iter_1), snd.new_at(*iter_2));
					if (iter_cmp != std::strong_ordering::equal) {
						return iter_cmp;
					}
				}
				return fst_vec.size() <=> snd_vec.size();
			} break;
			case PnType(Literal::variable): {
				return compare_char_vecs(*fst, *snd);
			} break;
			case PnType(Literal::complex): {
				return compare_complex(*fst, *snd);
			} break;
			case PnType(PnNode::tree_match): {
				return fst->tree_match.match_data_idx <=> snd->tree_match.match_data_idx;
			} break;
			case PnType(PnNode::value_match): {
				return fst->value_match.match_data_idx <=> snd->value_match.match_data_idx;
			} break;
			case PnType(PnNode::value_proxy):
			case PnType(MultiPn::summands): 
			case PnType(MultiPn::factors): 
			case PnType(MultiPn::params): 
				return fst.index <=> snd.index;
			}
		} //compare

		void sort(const MutPnRef ref) 
		{
			const auto sort_variadic = [](const MutPnRef ref) {
				const auto compare_function = [&](const PnIdx lhs, const PnIdx rhs) {
					return pn_tree::compare(PnRef(*ref.store, lhs), PnRef(*ref.store, rhs)) == std::strong_ordering::less;
				};

				if (ref.type.is<Comm>()) {
					PnIdxVector& operation = *ref;
					std::sort(operation.begin(), operation.end(), compare_function);
				}
				return fold::Void{};
			};

			fold::simple_fold<fold::Void>(ref, sort_variadic);
		} //sort

	} //namespace pn_tree

	namespace match {

		bool permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data)
		{
			if (pn_ref.type.is<MathType>() && pn_ref.type != ref.type) {
				return false;
			}
			switch (pn_ref.type) {
			case PnType(NamedFn{}): {
				const CharVector& name = fn::named_fn_name(ref);
				const CharVector& pn_name = fn::named_fn_name(pn_ref);
				if (std::string_view(name.data(), name.size()) != std::string_view(pn_name.data(), pn_name.size())) {
					return false;
				}
			} [[fallthrough]];
			default: {
				assert(pn_ref.type.is<Function>());
				if (pn_ref.type.is<Comm>()) {
					const bool params_at_back = pn_ref->parameters.back().get_type() == MultiPn::params;
					if (pn_ref->parameters.size() - params_at_back > ref->parameters.size()) {  //params can also match nothing -> subtract 1 then
						return false;
					}
					return find_matching_permutation(pn_ref, ref, match_data, 0u, 0u);
				}
				else if (pn_ref.type.is<Fn>()) {
					const PnIdxVector& pn_range = fn::range(pn_ref);
					const IndexVector& range = fn::range(ref);
					auto pn_iter = pn_range.begin();
					const auto pn_stop = pn_range.end();
					auto iter = range.begin();
					for (; pn_iter != pn_stop; ++pn_iter, ++iter) { //iter and pn_iter both go over same number of params
						if (!match::permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
							return false;
						}
					}
					return true;
				}
				else {
					assert(pn_ref.type.is<NonComm>() || pn_ref.type.is<NamedFn>());
					const PnIdxVector& pn_range = fn::range(pn_ref);
					const IndexVector& range = fn::range(ref);
					auto pn_iter = pn_range.begin();
					auto iter = range.begin();
					const auto pn_stop = pn_range.end();
					const auto stop = range.end();
					for (; pn_iter != pn_stop && iter != stop; ++pn_iter, ++iter) {
						if (pn_iter->get_type().is<MultiPn>()) {
							goto found_multi_pn;
						}
						else if (!match::permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
							return false;
						}
					}
					if (iter == stop) {
						if (pn_iter->get_type() == MultiPn::params) {
							goto found_multi_pn;
						}
						return pn_iter == pn_stop;
					}
					return false;

				found_multi_pn:
					auto& info = match_data.multi_info(pn_iter->get_index());
					const BitSet64 first_pn_elems_many_true = (1ull << (pn_range.size() - 1u)) - 1u; //more precisly: last elem not counted, as it is MultiPn 
					for (std::uint32_t i = 0u; i < pn_range.size() - 1u; i++) {
						info.match_positions[i] = i;
					}
					info.match_idx = ref.typed_idx();
					return true;
				}
			} break;
			case PnType(Literal::variable): {
				const CharVector& var = *ref;
				const CharVector& pn_var = *pn_ref;
				return std::string_view(var.data(), var.size()) == std::string_view(pn_var.data(), pn_var.size());
			} break;
			case PnType(Literal::complex): {
				const Complex& complex = *ref;
				const Complex& pn_complex = *pn_ref;
				return compare_complex(complex, pn_complex) == std::strong_ordering::equal;
			} break;
			case PnType(PnNode::tree_match): {
				const TreeMatchVariable& var = *pn_ref;
				if (!meets_restriction(ref, var.restr)) {
					return false;
				}
				auto& match_info = match_data.info(var);
				if (match_info.is_set()) {
					return tree::compare(ref, ref.new_at(match_info.match_idx)) == std::strong_ordering::equal;
				}
				else {
					match_info.match_idx = ref.typed_idx();
					match_info.responsible = pn_ref.typed_idx();
					return true;
				}
			} break;
			case PnType(PnNode::value_match): {
				if (ref.type != Literal::complex) { //only this test allows us to pass *ref to evaluate this_value
					return false;
				}
				const ValueMatchVariable& var = *pn_ref;
				auto& match_info = match_data.info(var);
				const OptionalComplex this_value = pn_tree::eval_value_match(pn_ref.new_at(var.mtch_idx), *ref);
				if (!this_value || !has_form(*this_value, var.form)) {
					return false;
				}
				else if (match_info.is_set()) {
					return this_value.val == match_info.value;
				}
				else {
					match_info.value = *this_value;
					match_info.responsible = pn_ref.typed_idx();
					return true;
				}
			} break;
			case PnType(PnNode::value_proxy): //may only be encountered in pn_tree::eval_value_match (as value_match does no permutation_equals call)
				assert(false);
				return false;
			case PnType(MultiPn::summands): //not expected in matching side of pattern, only in replacement side
				[[fallthrough]];
			case PnType(MultiPn::factors): //not expected in matching side of pattern, only in replacement side
				[[fallthrough]];
			case PnType(MultiPn::params): //assumed to be handeled only as param of named_fn or ordered elements in Variadic 
				assert(false);
				return false;
			}
		} //permutation_equals

		void reset_own_matches(const pattern::UnsavePnRef pn_ref, MatchData& match_data)
		{
			switch (pn_ref.type) {
			default:
				if (pn_ref.type.is<Function>()) {
					for (const PnIdx elem : fn::range(pn_ref)) {
						reset_own_matches(pn_ref.new_at(elem), match_data);
					}
				}
				break;
			case PnType(PnNode::tree_match): {
				SharedTreeDatum& info = match_data.info(pn_ref->tree_match);
				if (info.responsible == pn_ref.typed_idx()) {
					info = SharedTreeDatum();
				}
			} break;
			case PnType(PnNode::value_match): {
				SharedValueDatum& info = match_data.info(pn_ref->value_match);
				if (info.responsible == pn_ref.typed_idx()) {
					info = SharedValueDatum();
				}
			} break;
			case PnType(MultiPn::summands): //nothing to do for these (done by variadic)
				break;
			case PnType(MultiPn::factors):
				break;
			case PnType(MultiPn::params):
				break;
			}
		} //reset_own_matches

		bool subsequent_permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data)
		{
			if (!pn_ref.type.is<Function>()) {
				return false; //can not rematch at all
			}
			if (pn_ref.type.is<Comm>()) {
				SharedVariadicDatum& variadic_datum = match_data.variadic_data.at(pn_ref.index);
				const PnIdxVector& pn_params = *pn_ref;
				assert(variadic_datum.match_idx == ref.typed_idx()); //assert pn_ref is currently matched in ref
				std::uint32_t pn_i = pn_params.size() - 1u;
				if (pn_params[pn_i].get_type().is<MultiPn>()) {
					if (pn_i == 0u) {
						return false;
					}
					pn_i--;
				}
				assert(!pn_params[pn_i].get_type().is<MultiPn>()); //there may only be a single one in each variadic
				reset_own_matches(pn_ref, match_data);
				const std::uint32_t last_haystack_k = variadic_datum.match_positions[pn_i];
				return find_matching_permutation(pn_ref, ref, match_data, pn_i, last_haystack_k + 1u);
			}
			else {
				const auto& pn_range = fn::range(pn_ref);
				const auto& range = fn::range(ref);
				const auto pn_stop = pn_range.end();
				auto pn_iter = pn_range.begin();
				auto iter = range.begin();
				for (; pn_iter != pn_stop; ++pn_iter, ++iter) { //iter and pn_iter both go over same number of params
					if (match::subsequent_permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
						return true;
					}
				}
				return false;
			}
		} //subsequent_permutation_equals

		bool find_matching_permutation(const pattern::UnsavePnRef pn_ref, const UnsaveRef haystack_ref, MatchData& match_data, std::uint32_t pn_i, std::uint32_t haystack_k)
		{
			assert(pn_ref.type == haystack_ref.type && (haystack_ref.type.is<Comm>()));

			const PnIdxVector& pn_params = *pn_ref;
			const IndexVector& haystack_params = *haystack_ref;

			assert(std::is_sorted(haystack_params.begin(), haystack_params.end(), [&](auto lhs, auto rhs) {
				return tree::compare(haystack_ref.new_at(lhs), haystack_ref.new_at(rhs)) == std::strong_ordering::less;
				}));

			SharedVariadicDatum& variadic_datum = match_data.variadic_data.at_or_insert(pn_ref.index);
			variadic_datum.match_idx = haystack_ref.typed_idx();

			BitVector currently_matched = BitVector(haystack_params.size()); //one bit for each element in haystack_params
			for (std::uint32_t i = 0u; i < pn_i; i++) {
				currently_matched.set(variadic_datum.match_positions[i]);
			}

			while (pn_i < pn_params.size()) {
				const PnType pn_i_type = pn_params[pn_i].get_type();
				assert((!pn_i_type.is<MultiPn>() || pn_i_type == MultiPn::params) && "only MultiPn expected in lhs is params");
				if (pn_i_type == MultiPn::params) [[unlikely]] { //also summands and factors are matched as params
					assert(pn_i + 1ull == pn_params.size() && "MultiPn is only valid as last element -> only one per variadic");
					assert(&match_data.multi_info(pn_params[pn_i].get_index()) == &variadic_datum); //just out of paranoia
					return true;
				}
				else if (pn_i_type.is<MathType>() || pn_i_type == PnNode::value_match) {
					const pattern::UnsavePnRef pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					const int pn_i_generality = generality(pn_i_type.to<MathType>());
					for (; haystack_k < haystack_params.size(); haystack_k++) {
						//static_assert(generality(PnNode::value_match) > generality(Literal::complex));
						if (pn_i_generality < generality(haystack_params[haystack_k].get_type())) {
							goto rematch_last_pn_i; //specimen of pn_i_type in haystack sorted in front of k -> no hope left finding them here or later
						}
						if (currently_matched.test(haystack_k)) {
							continue; //ignore parts of haystack currently already associated with elements in pattern
						}
						if (match::permutation_equals(pn_i_ref, haystack_ref.new_at(haystack_params[haystack_k]), match_data)) {
							goto prepare_next_pn_i; //next while iteration
						}
						reset_own_matches(pn_i_ref, match_data);
					}
				}
				else {
					assert(pn_i_type == PnNode::tree_match); //other may not be encoountered in this function
					const pattern::UnsavePnRef pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					const auto& match_info = match_data.info(pn_i_ref->tree_match);

					if (match_info.is_set()) { //binary search for needle over part of haystack allowed to search in
						const UnsaveRef needle = haystack_ref.new_at(match_info.match_idx);
						std::uint32_t search_begin = haystack_k;
						std::uint32_t search_end = haystack_params.size();

						while (search_begin < search_end) {
							const std::uint32_t midpoint = std::midpoint(search_begin, search_end); //rounded torwards search_begin
							haystack_k = midpoint; //only call tree::compare where not currently matched
							while (currently_matched.test(haystack_k) && haystack_k < search_end) {
								haystack_k++;
							}
							if (haystack_k == search_end) {
								search_end = midpoint;
								continue;
							}

							const UnsaveRef search_ref = haystack_ref.new_at(haystack_params[haystack_k]);
							const std::strong_ordering cmp = tree::compare(search_ref, needle);
							if (cmp == std::strong_ordering::equal) {
								goto prepare_next_pn_i;
							}
							else if (cmp == std::strong_ordering::less) {
								search_begin = haystack_k + 1u;
							}
							else {
								assert(cmp == std::strong_ordering::greater);
								search_end = midpoint;
							}
						}
					}
					else { //match info is not yet set -> test all
						for (; haystack_k < haystack_params.size(); haystack_k++) {
							if (currently_matched.test(haystack_k)) {
								continue;
							}
							if (match::permutation_equals(pn_i_ref, haystack_ref.new_at(haystack_params[haystack_k]), match_data)) {
								goto prepare_next_pn_i;
							}
						}
					}
				}

			rematch_last_pn_i:
				if (pn_i == 0u) {
					return false;
				}
				else {
					//this while loop iteration tried matching some element after the first in pattern (and failed)
					// -> perhaps an element preceding the current one could be matched differently
					// -> try that
					pn_i--;
					const pattern::UnsavePnRef pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					haystack_k = variadic_datum.match_positions[pn_i];
					//try rematching the last successfully matched element in pattern with same part it matched with previously
					// (but it can not match any way that was already tried, duh)
					if (subsequent_permutation_equals(pn_i_ref, haystack_ref.new_at(haystack_params[haystack_k]), match_data)) {
						//success -> try matching the element not matchable this while iteration with (perhaps) now differenty set match variables
						pn_i++;
						haystack_k = 0u;
					}
					else {
						//could not rematch last successfully matched element with same element in haystack 
						//  -> try succeeding elements in haystack in next loop iteration
						reset_own_matches(pn_i_ref, match_data);
						currently_matched.reset(haystack_k);
						haystack_k++;
					}
				}
				continue;
			prepare_next_pn_i:
				currently_matched.set(haystack_k);
				variadic_datum.match_positions[pn_i] = haystack_k;
				haystack_k = 0u;
				pn_i++;
			}
			return pn_params.size() == haystack_params.size();
		} //find_matching_permutation

		MathIdx copy(const pattern::UnsavePnRef pn_ref, const MatchData& match_data, const MathStore& src_store, MathStore& dst_store)
		{
			switch (pn_ref.type) {
			default: {
				assert(pn_ref.type.is<Function>());
				StupidBufferVector<MathIdx, 12> dst_parameters;
				for (const PnIdx pn_param : fn::range(pn_ref)) {
					if (pn_param.get_type() == MultiPn::params) { //summands and factors need stay their type (summands always to sum...)
						const SharedVariadicDatum& info = match_data.multi_info(pn_param.get_index());
						const auto src_range = fn::save_range(Ref(src_store, info.match_idx));
						const auto src_stop = end(src_range);
						for (auto src_iter = begin(src_range); src_iter != src_stop; ++src_iter) {
							if (!info.index_matched(src_iter.array_idx)) {
								const MathIdx dst_param = tree::copy(Ref(src_store, *src_iter), dst_store); //call normal copy!
								dst_parameters.push_back(dst_param);
							}
						}
					}
					else {
						const MathIdx dst_param = match::copy(pn_ref.new_at(pn_param), match_data, src_store, dst_store);
						dst_parameters.push_back(dst_param);
					}
				}
				if (pn_ref.type.is<NamedFn>()) {
					const CharVector& name_ref = fn::named_fn_name(pn_ref);
					std::string name = std::string(name_ref.begin(), name_ref.end());
					return fn::build_named_fn<MathType>(dst_store, std::move(name), dst_parameters);
				}
				else {
					return MathIdx(IndexVector::build(dst_store, dst_parameters), pn_ref.type.to<MathType>());
				}
			} break;
			case PnType(Literal::variable): {
				const CharVector& src_var = *pn_ref;
				const auto src_name = std::string(src_var.data(), src_var.size());
				const std::size_t dst_index = CharVector::build(dst_store, src_name);
				return MathIdx(dst_index, pn_ref.type.to<MathType>());
			} break;
			case PnType(Literal::complex): {
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = pn_ref->complex; //bitwise copy
				return MathIdx(dst_index, pn_ref.type.to<MathType>());
			} break;
			case PnType(PnNode::tree_match): {
				const SharedTreeDatum& info = match_data.info(pn_ref->tree_match);
				assert(info.is_set());
				return tree::copy(Ref(src_store, info.match_idx), dst_store); //call to different copy!
			} break; 
			case PnType(PnNode::value_match):  //value_match is not expected in rhs, only value_proxy
				assert(false);
				return MathIdx();
			case PnType(PnNode::value_proxy): {
				const Complex& val = match_data.value_match_data[pn_ref.index].value;
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = val;
				return MathIdx(dst_index, MathType(Literal::complex));
			} break;
			case PnType(MultiPn::summands):
				[[fallthrough]];
			case PnType(MultiPn::factors): {
				StupidBufferVector<MathIdx, 12> dst_parameters;
				const SharedVariadicDatum& info = match_data.multi_info(pn_ref.index);
				const auto src_range = fn::save_range(Ref(src_store, info.match_idx));
				const auto src_stop = end(src_range);
				for (auto src_iter = begin(src_range); src_iter != src_stop; ++src_iter) {

					if (!info.index_matched(src_iter.array_idx)) {
						const MathIdx dst_param = tree::copy(Ref(src_store, *src_iter), dst_store); //call normal copy!
						dst_parameters.push_back(dst_param);
					}
				}

				const std::uint32_t res_idx = IndexVector::build(dst_store, dst_parameters);
				return MathIdx(res_idx, pn_ref.type == MultiPn::summands ? MathType(Comm::sum) : MathType(Comm::product));
			} break;
			case PnType(MultiPn::params):  //already handeled in named_fn
				assert(false);
				return MathIdx();
			}
		} //copy

		std::optional<MathIdx> match_and_replace(const pattern::UnsavePnRef from, const pattern::UnsavePnRef to, const MutRef ref)
		{
			MatchData match_data;
			if (match::permutation_equals(from, ref, match_data)) {
				MathStore copy_buffer;
				copy_buffer.reserve(32u);
				const MathIdx buffer_head = match::copy(to, match_data, *ref.store, copy_buffer);
				tree::free(ref);
				const MathIdx result_head = tree::copy(Ref(copy_buffer, buffer_head), *ref.store);
				return { result_head };
			}
			return {};
		} //match_and_replace

		std::pair<std::optional<MathIdx>, bool> recursive_match_and_replace(const pattern::UnsavePnRef in, const pattern::UnsavePnRef out, const MutRef ref)
		{
			if (ref.type.is<Function>()) {
				auto range = fn::range(ref);
				const auto stop = end(range);
				for (auto iter = begin(range); iter != stop; ++iter) {
					const auto [new_elem, matched_deeper] = recursive_match_and_replace(in, out, ref.new_at(*iter));
					if (new_elem) {
						*iter = *new_elem;
						return std::make_pair(std::nullopt, true);
					}
					else if (matched_deeper) {
						return std::make_pair(std::nullopt, true);
					}
				}
			}
			return std::make_pair(match_and_replace(in, out, ref), false);
		} //recursive_match_and_replace

	} //namespace match

} //namespace bmath::intern::pattern
