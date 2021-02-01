
#include <iostream>
#include <cfenv>

#include "pattern.hpp"
#include "ioPattern.hpp"
#include "ioArithmetic.hpp"
#include "fold.hpp"


namespace bmath::intern::pattern {


	bool in_domain(const Complex& nr, const Domain domain)
	{
		constexpr double max_save_int = 9007199254740991; //== 2^53 - 1, largest integer explicitly stored in double

		const double re = nr.real();
		const double im = nr.imag();

		bool accept = true;
		switch (domain) {
		case Domain::natural:   accept &= re > 0.0;                       [[fallthrough]];
		case Domain::natural_0: accept &= re >= 0.0;                      [[fallthrough]];
		case Domain::integer:   accept &= re - std::int64_t(re) == 0.0;
			                    accept &= (std::abs(re) <= max_save_int); [[fallthrough]];
		case Domain::real:      accept &= im == 0.0;                      [[fallthrough]];
		case Domain::complex:
			return accept;

		case Domain::negative:      return re  < 0.0 && im == 0.0;
		case Domain::positive:      return re  > 0.0 && im == 0.0;
		case Domain::not_negative:  return re >= 0.0 && im == 0.0;
		case Domain::not_positive:  return re <= 0.0 && im == 0.0;
		default:
			assert(false);
			BMATH_UNREACHABLE;
			return false;
		}
	} //has_form

	bool meets_restriction(const UnsaveRef ref, const TreeMatchOwning restr)
	{
		if (restr == Restriction::any) {
			return true;
		}
		switch (restr) {
		case TreeMatchOwning(Restriction::variable):
			return ref.type == Literal::variable;
		case TreeMatchOwning(Restriction::no_val):
			return ref.type != Literal::complex;
		case TreeMatchOwning(Restriction::nn1):
			return (ref.type != Literal::complex) || (ref->complex != -1.0);
		default:
			assert(restr.is<Domain>());
			if (ref.type == Literal::complex) {
				return in_domain(*ref, restr.to<Domain>());
			}
			else {
				return false;
			}
		}
	} //meets_restriction


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
			this->lhs_head = match::apply_rule_range(bubble_up_value_match.data(), bubble_up_value_match.data() + bubble_up_value_match.size(), this->lhs_mut_ref());

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
			this->lhs_head = match::apply_rule_range(invert_match_subtree.data(), invert_match_subtree.data() + invert_match_subtree.size(), this->lhs_mut_ref());
			//note it is sometimes desired to use a ValueMatchVariable in a context not allowing it to swallow any of its surroundings. 
			//combining both rulesets would therefore result in an infinite loop in that case, as the very first and very last pattern convert between each other.
		}

		for (const auto& multi_match : match_variables_table.multi_table) {
			throw_if(multi_match.lhs_count > 1u, "pattern only allows single use of each MultiParams in lhs.");
		}

		//sorting and combining is done after rearanging value match to allow constructs 
		//  like "a :real, b | (a+2)+b = ..." to take summands / factors into their value_match match part
		this->lhs_head = tree::establish_basic_order(this->lhs_mut_ref());
		this->rhs_head = tree::establish_basic_order(this->rhs_mut_ref());

		{ //add implicit MultiParams{} if outermost type of lhs is associative and commutative
			const MathType head_type = this->lhs_head.get_type();
			if (head_type.is<Comm>() && fn::is_associative(head_type)) {
				const IndexVector& head_variadic = this->store.at(this->lhs_head.get_index());
				const auto multi = std::find_if(head_variadic.begin(), head_variadic.end(),
					[&](const MathIdx idx) { return math_rep::IntermediateMultiMatch::cast(Ref(this->store, idx)).has_value(); });
				if (multi == head_variadic.end()) {
					{ //adjust lhs
						const MathIdx new_multi = math_rep::IntermediateMultiMatch::build(this->store, match_variables_table.multi_table.size(),
							Multi(head_type.to<Variadic>()));
						const std::size_t new_lhs_head_idx = this->store.allocate_one();
						this->store.at(new_lhs_head_idx) = IndexVector({ this->lhs_head, new_multi });
						this->lhs_head = MathIdx(new_lhs_head_idx, head_type);
						this->lhs_head = tree::establish_basic_order(this->lhs_mut_ref());
					}
					{ //adjust rhs
						const MathIdx new_multi = math_rep::IntermediateMultiMatch::build(this->store, match_variables_table.multi_table.size(),
							Multi(head_type.to<Variadic>()));
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
			IntermediateRewriteRule intermediate = IntermediateRewriteRule(name, convert);
			assert(tree::valid_storage(intermediate.store, { intermediate.lhs_head, intermediate.rhs_head }));

			this->lhs_head = pn_tree::intermediate_to_pattern(intermediate.lhs_ref(), this->store, Side::lhs, convert, PnType::COUNT);
			this->rhs_head = pn_tree::intermediate_to_pattern(intermediate.rhs_ref(), this->store, Side::rhs, convert, PnType::COUNT);
			pn_tree::sort(this->lhs_mut_ref());
			pn_tree::sort(this->rhs_mut_ref());
		}
		{ //adjusting MultiParams indices to SharedVariadicDatum index of each MulitPn on lhs and setting .match_data_idx of VariadicMetaData

			//has to be filled in same order as MatchData::variadic_data
			//index of element in old_multis equals value of corrected MultiParams occurence (plus the type)
			std::vector<PnIdx> old_multis;
			const auto catalog_lhs_occurences = [&old_multis](const MutPnRef head) {
				struct Acc
				{
					std::vector<PnIdx>* old_multis;
					std::uint32_t own_idx;

					constexpr Acc(const MutPnRef ref, std::vector<PnIdx>* new_old_multis) noexcept
						:old_multis(new_old_multis), own_idx(-1u)
					{
						if (ref.type.is<Variadic>()) { //these may contain MultiParams -> these have SharedVariadicDatum entry 
							this->own_idx = this->old_multis->size(); //new last element 
							variadic_meta_data(ref).match_data_idx = this->own_idx; 
							this->old_multis->emplace_back(PnIdx{}); //becomes only valid element if consume finds MultiParams
						}
					}

					void consume(const PnIdx child)
					{
						if (child.get_type().is<MultiParams>()) {
							if (this->own_idx == -1) throw std::exception("found MultiParams in unexprected place");
							this->old_multis->at(this->own_idx) = child;
						}
					}

					auto result() noexcept { return PnIdx{}; } //caution: only works as long as no multi is represented as Function
				};
				(void)fold::tree_fold<PnIdx, Acc>(head, [](const MutPnRef ref) { return ref.typed_idx(); }, &old_multis);
			};
			catalog_lhs_occurences(this->lhs_mut_ref());

			const auto replace_occurences = [&old_multis](const MutPnRef head, const bool test_lhs) -> bool {
				const auto check_function_params = [&old_multis, test_lhs](const MutPnRef ref) -> fold::FindTrue {
					if (ref.type.is<Function>()) {
						for (auto& param : fn::unsave_range(ref)) {
							if (param.get_type().is<MultiParams>()) {
								if (!ref.type.is<Variadic>() && test_lhs) { //only Variadic may carry MultiParams in lhs
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
			throw_if(replace_occurences(this->lhs_mut_ref(), true), "MultiParams in unexpected place in lhs");
			throw_if(replace_occurences(this->rhs_mut_ref(), false), "MultiParams only referenced in rhs");
		}		
		{
			const auto contains_illegal_value_match = [](const PnRef head) -> bool {
				const auto inspect_variadic = [](const PnRef ref) -> fold::FindTrue {
					if (ref.type == Comm::sum || ref.type == Comm::product) {
						std::size_t nr_value_matches = 0u;
						for (const PnIdx elem : fn::range(ref)) {
							nr_value_matches += (elem.get_type().is<ValueMatch>());
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
							nr_multi_matches += elem.get_type().is<MultiParams>();
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
						const std::uint32_t multi_at_back = ref->parameters.back().get_type().is<MultiParams>();
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
						:acc(ref.type.is<ValueMatch>() ?
							std::numeric_limits<int>::min() : //subterms of value_match dont count -> initialize negative
							ref.type.is<Variadic>()) //count only these
					{}

					void consume(const int child_size) noexcept { this->acc += child_size; }
					auto result() noexcept { return std::max(this->acc, 0); } //always this->acc if type was other than value_match
				};
				const int variadic_count = fold::tree_fold<std::size_t, Acc>(head, [](auto) { return 0; });
				return variadic_count > match::MatchData::max_variadic_count;
			};
			throw_if(contains_to_many_variadics(this->lhs_ref()), "lhs contains to many variadic functions");
		}
		{ //set all but first TreeMatch of one index to TreeMatchNonOwning
			const auto disappropriate_tree_match_variables = [](const MutPnRef ref) {
				//tree_match_occurences[i] counts how often tree match with index i has been encountered in tree
				std::array<int, match::MatchData::max_tree_match_count> tree_match_occurences = {};

				const auto disappropriate_tree_match_variable = [&tree_match_occurences](const MutPnRef ref) -> PnIdx {
					if (ref.type.is<TreeMatchOwning>()) {	
						if (tree_match_occurences[ref.index]++) {
							return PnIdx(ref.index, TreeMatchNonOwning{});
						}
					}
					return ref.typed_idx();
				};
				const PnIdx new_head = fold::mutate_fold<PnIdx>(ref, disappropriate_tree_match_variable);
				assert(new_head == ref.typed_idx()); //no top level match variables allowed!
			};
			disappropriate_tree_match_variables(this->lhs_mut_ref());
		}
		{ //set all but first ValueMatch of one index to ValueMatch::non_owning
			const auto disappropriate_value_match_variables = [](const MutPnRef ref) {
				//tree_match_occurences[i] counts how often value match with .match_data_idx i has been encountered in tree
				std::array<int, match::MatchData::max_value_match_count> value_match_occurences = {};

				const auto disappropriate_value_match_variable = [&value_match_occurences](const MutPnRef ref) -> PnIdx {
					if (ref.type == ValueMatch::owning) {
						const ValueMatchVariable& var = *ref;
						if (value_match_occurences[var.match_data_idx]++) {
							return PnIdx(ref.index, ValueMatch::non_owning);
						}
					}
					return ref.typed_idx();
				};
				const PnIdx new_head = fold::mutate_fold<PnIdx>(ref, disappropriate_value_match_variable);
				assert(new_head == ref.typed_idx()); //no top level match variables allowed!
			};
			disappropriate_value_match_variables(this->lhs_mut_ref());
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
			case PnType(ValueProxy{}):
				return start_val;
			}
		} //eval_value_match

		PnIdx intermediate_to_pattern(const UnsaveRef src_ref, PnStore& dst_store, const Side side, const Convert convert, const PnType parent_type)
		{
			switch (src_ref.type) {
			case MathType(NamedFn{}): {
				const std::string_view name = fn::named_fn_name(src_ref);
				if (const auto tree_match = math_rep::IntermediateTreeMatch::cast(src_ref)) {
					return PnIdx(tree_match->match_data_idx(), tree_match->restr());
				}
				if (const auto multi_match = math_rep::IntermediateMultiMatch::cast(src_ref)) {
					const Variadic multi_parent = multi_match->type();
					if (multi_parent == parent_type) {
						return PnIdx(multi_match->index(), MultiParams{});
					}
					else {
						assert(side == Side::rhs);
						const std::size_t dst_index = dst_store.allocate_one();
						dst_store.at(dst_index) = PnIdxVector{ PnIdx(multi_match->index(), MultiParams{}) };
						return PnIdx(dst_index, multi_parent);
					}
				}

				if (convert == Convert::all) {
					if (const auto value_match = math_rep::IntermediateValueMatch::cast(src_ref)) {
						if (side == Side::lhs) {
							const PnIdx mtch_idx = intermediate_to_pattern(src_ref.new_at(value_match->mtch_idx()), dst_store, side, convert, ValueMatch::owning);
							const std::uint32_t match_data_idx = value_match->match_data_idx();
							const Domain domain = value_match->domain();

							const std::size_t dst_index = dst_store.allocate_one();
							dst_store.at(dst_index) = ValueMatchVariable(mtch_idx, match_data_idx, domain);
							return PnIdx(dst_index, ValueMatch::owning);
						}
						else {
							assert(side == Side::rhs);

							const auto value_proxy = math_rep::IntermediateValueProxy::cast(src_ref.new_at(value_match->mtch_idx()));
							assert(value_proxy);
							return PnIdx(value_proxy->index(), ValueProxy{});
						}
					}
					if (const auto value_proxy = math_rep::IntermediateValueProxy::cast(src_ref)) {
						assert(side == Side::lhs);
						return PnIdx(value_proxy->index(), ValueProxy{});
					}
				}
			} [[fallthrough]]; //if NamedFn did not represent a match variable just keep it as is
			default: {
				assert(src_ref.type.is<Function>());
				StupidBufferVector<PnIdx, 12> dst_parameters;
				for (const MathIdx src_param : fn::range(src_ref)) {
					const PnIdx dst_param = intermediate_to_pattern(src_ref.new_at(src_param), dst_store, side, convert, src_ref.type);
					dst_parameters.push_back(dst_param);
				}
				if (src_ref.type.is<NamedFn>()) {
					const CharVector& name_ref = fn::named_fn_name(src_ref);
					return fn::build_named_fn<PnType>(dst_store, name_ref, dst_parameters);
				}
				else if (src_ref.type.is<Variadic>()) { //allocate one extra for metadata
					const std::size_t alloc_capacity = PnIdxVector::smallest_fit_capacity(dst_parameters.size());
					const std::size_t alloc_idx = dst_store.allocate_n(1u + PnIdxVector::_node_count(alloc_capacity)) + 1u; //1u for metadata
					PnIdxVector::emplace(dst_store.at(alloc_idx), dst_parameters, alloc_capacity);
					dst_store.at(alloc_idx - 1u) = VariadicMetaData{}; //still needs to be set to corrent values
					return PnIdx(alloc_idx, src_ref.type);
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
				if (fst.type.is<Proxy>()) {
					return fst.index <=> snd.index;
				}
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
			case PnType(ValueMatch::non_owning):
				[[fallthrough]];
			case PnType(ValueMatch::owning): {
				return fst->value_match.match_data_idx <=> snd->value_match.match_data_idx;
			} break;
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
				if (pn_ref->parameters.size() != ref->parameters.size()) {
					return false;
				}
				const CharVector& name = fn::named_fn_name(ref);
				const CharVector& pn_name = fn::named_fn_name(pn_ref);
				if (std::string_view(name) != std::string_view(pn_name)) {
					return false;
				}
			} [[fallthrough]];
			default: {
				if (pn_ref.type.is<TreeMatchOwning>()) {
					if (!meets_restriction(ref, pn_ref.type.to<TreeMatchOwning>())) {
						return false;
					}
					auto& match_info = match_data.tree_info(pn_ref.index);
					assert(!match_info.is_set());
					match_info.match_idx = ref.typed_idx();
					return true;
				}
				assert(pn_ref.type.is<Function>());
				if (pn_ref.type.is<Comm>()) {
					const bool params_at_back = pn_ref->parameters.back().get_type() == MultiParams{};
					if (pn_ref->parameters.size() - params_at_back > ref->parameters.size()) {  //params can also match nothing -> subtract 1 then
						return false;
					}
					return find_matching_permutation(pn_ref, ref, match_data, 0u, 0u);
				}
				else if (pn_ref.type.is<Fn>() || pn_ref.type.is<NamedFn>()) {
					assert(pn_ref->parameters.size() == ref->parameters.size()); //true for NamedFn because we tested earlier and always true for Fn 
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
					assert(pn_ref.type.is<NonComm>());
					const PnIdxVector& pn_range = fn::range(pn_ref);
					const IndexVector& range = fn::range(ref);
					auto pn_iter = pn_range.begin();
					auto iter = range.begin();
					const auto pn_stop = pn_range.end();
					const auto stop = range.end();
					for (; pn_iter != pn_stop && iter != stop; ++pn_iter, ++iter) {
						if (pn_iter->get_type().is<MultiParams>()) {
							goto found_multi_pn;
						}
						else if (!match::permutation_equals(pn_ref.new_at(*pn_iter), ref.new_at(*iter), match_data)) {
							return false;
						}
					}
					if (iter == stop) {
						if (pn_iter->get_type() == MultiParams{}) {
							goto found_multi_pn;
						}
						return pn_iter == pn_stop;
					}
					return false;

				found_multi_pn:
					auto& info = match_data.multi_info(pn_iter->get_index());
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
				return std::string_view(var) == std::string_view(pn_var);
			} break;
			case PnType(Literal::complex): {
				const Complex& complex = *ref;
				const Complex& pn_complex = *pn_ref;
				return compare_complex(complex, pn_complex) == std::strong_ordering::equal;
			} break;
			case PnType(TreeMatchNonOwning{}): {
				auto& match_info = match_data.tree_info(pn_ref.index);
				assert(match_info.is_set());
				return tree::compare(ref, ref.new_at(match_info.match_idx)) == std::strong_ordering::equal;
			} break;
			case PnType(ValueMatch::owning): {
				if (ref.type != Literal::complex) { //only this test allows us to pass *ref to evaluate this_value
					return false;
				}
				const ValueMatchVariable& var = *pn_ref;
				auto& match_info = match_data.info(var);
				assert(!match_info.is_set());
				const OptionalComplex this_value = pn_tree::eval_value_match(pn_ref.new_at(var.mtch_idx), *ref);
				if (!this_value || !in_domain(*this_value, var.domain)) {
					return false;
				}
				else {
					match_info.value = *this_value;
					return true;
				}
			} break;
			case PnType(ValueMatch::non_owning): {
				if (ref.type != Literal::complex) { //only this test allows us to pass *ref to evaluate this_value
					return false;
				}
				const ValueMatchVariable& var = *pn_ref;
				const auto& match_info = match_data.info(var);
				assert(match_info.is_set());
				const OptionalComplex this_value = pn_tree::eval_value_match(pn_ref.new_at(var.mtch_idx), *ref);
				if (!this_value || !in_domain(*this_value, var.domain)) {
					return false;
				}
				else {
					return *this_value == match_info.value;
				}
			} break;
			case PnType(ValueProxy{}): //may only be encountered in pn_tree::eval_value_match (as value_match does no permutation_equals call)
				[[fallthrough]];
			case PnType(MultiParams{}): //assumed to be handeled only as param of named_fn or ordered elements in Variadic 
				assert(false);
				BMATH_UNREACHABLE;
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
				else if (pn_ref.type.is<TreeMatchOwning>()) {
					SharedTreeDatum& info = match_data.tree_info(pn_ref.index);
					info = SharedTreeDatum();
				}
				break;
			case PnType(ValueMatch::owning): {
				SharedValueDatum& info = match_data.info(pn_ref->value_match);
				info = SharedValueDatum();
			} break;
			case PnType(MultiParams{}):
				break;
			}
		} //reset_own_matches

		bool subsequent_permutation_equals(const pattern::UnsavePnRef pn_ref, const UnsaveRef ref, MatchData& match_data)
		{
			if (!pn_ref.type.is<Function>()) {
				return false; //can not rematch at all
			}
			if (pn_ref.type.is<Comm>()) {
				SharedVariadicDatum& variadic_datum = match_data.variadic_data[variadic_meta_data(pn_ref).match_data_idx];
				const PnIdxVector& pn_params = *pn_ref;
				assert(variadic_datum.match_idx == ref.typed_idx()); //assert pn_ref is currently matched in ref
				std::uint32_t pn_i = pn_params.size() - 1u;
				if (pn_params[pn_i].get_type().is<MultiParams>()) {
					if (pn_i == 0u) {
						return false;
					}
					pn_i--;
				}
				assert(!pn_params[pn_i].get_type().is<MultiParams>()); //there may only be a single one in each variadic
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

			const VariadicMetaData meta_data = variadic_meta_data(pn_ref);

			const PnIdxVector& pn_params = *pn_ref;
			const IndexVector& haystack_params = *haystack_ref;

			assert(std::is_sorted(haystack_params.begin(), haystack_params.end(), [&](auto lhs, auto rhs) {
				return tree::compare(haystack_ref.new_at(lhs), haystack_ref.new_at(rhs)) == std::strong_ordering::less;
				}));

			SharedVariadicDatum& variadic_datum = match_data.variadic_data[meta_data.match_data_idx];
			variadic_datum.match_idx = haystack_ref.typed_idx();

			BitVector currently_matched = BitVector(haystack_params.size()); //one bit for each element in haystack_params
			for (std::uint32_t i = 0u; i < pn_i; i++) {
				currently_matched.set(variadic_datum.match_positions[i]);
			}

			while (pn_i < pn_params.size()) {
				const PnType pn_i_type = pn_params[pn_i].get_type();
				if (pn_i_type.is<MultiParams>()) [[unlikely]] { //also summands and factors are matched as params
					assert(pn_i + 1ull == pn_params.size() && "MultiParams is only valid as last element -> only one per variadic");
					assert(&match_data.multi_info(pn_params[pn_i].get_index()) == &variadic_datum); //just out of paranoia
					return true;
				}
				else if (pn_i_type.is<MathType>() || pn_i_type.is<ValueMatch>()) {
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
					assert(pn_i_type.is<TreeMatch>()); //other may not be encoountered in this function
					const pattern::UnsavePnRef pn_i_ref = pn_ref.new_at(pn_params[pn_i]);
					const auto& match_info = match_data.tree_info(pn_i_ref.index);

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
				if (pn_ref.type.is<TreeMatchOwning>()) {
					const SharedTreeDatum& info = match_data.tree_info(pn_ref.index);
					assert(info.is_set());
					return tree::copy(Ref(src_store, info.match_idx), dst_store); //call to different copy!
				}
				assert(pn_ref.type.is<Function>());
				StupidBufferVector<MathIdx, 12> dst_parameters;
				for (const PnIdx pn_param : fn::range(pn_ref)) {
					if (pn_param.get_type() == MultiParams{}) { 
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
			case PnType(ValueProxy{}): {
				const Complex& val = match_data.value_match_data[pn_ref.index].value;
				const std::size_t dst_index = dst_store.allocate_one();
				dst_store.at(dst_index) = val;
				return MathIdx(dst_index, MathType(Literal::complex));
			} break;
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

		MathIdx apply_rule_range(const RewriteRule* const start, const RewriteRule* const stop, const MutRef ref)
		{
			MathIdx head = tree::establish_basic_order(ref);
		try_all_rules:
			for (auto rule = start; rule != stop; ++rule) {
				const auto [head_match, deeper_match] =
					recursive_match_and_replace(rule->lhs_ref(), rule->rhs_ref(), ref.new_at(head));
				if (head_match) {
					head = *head_match;
				}
				if (head_match || deeper_match) {
					head = tree::establish_basic_order(ref.new_at(head));
					//std::cout << " rule: " << rule->to_string() << "\n";
					//std::cout << "  ->   " << print::to_pretty_string(ref.new_at(head)) << "\n\n\n";
					goto try_all_rules;
				}
			}
			return head;
		}

	} //namespace match

} //namespace bmath::intern::pattern
