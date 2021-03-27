#pragma once

#include "pattern.hpp"
#include "utility/misc.hpp"

namespace bmath::intern::pattern {

	struct UnknownPnVar :SingleSumEnumEntry {};

	using Multi = OpaqueEnum<0, Variadic>;

	//intermediary used in build process
	using PnVariablesType = SumEnum<TreeMatchOwning, ValueDomain, Multi, UnknownPnVar>;

	struct TypeProps
	{
		PnVariablesType type = PnVariablesType(UnknownPnVar{});
		std::string_view name = "";
	};

	constexpr auto type_table = std::to_array<TypeProps>({
		{ Restriction::any                 , "any"           },
		{ Restriction::nn1                 , "nn1"           },
		{ Restriction::no_val              , "no_val"        },
		{ Restriction::symbol            , "symbol"      },
		{ TreeDomain(Domain::complex)      , "value"         }, //not to be mistaken for ValueDomain(Domain::complex)
		{ ValueDomain(Domain::natural)     , "nat"           },
		{ ValueDomain(Domain::natural_0)   , "nat0"          },
		{ ValueDomain(Domain::integer)     , "int"           },
		{ ValueDomain(Domain::real)        , "real"          },
		{ ValueDomain(Domain::complex)     , "complex"       }, //not to be mistaken for TreeDomain(Domain::complex)
		{ ValueDomain(Domain::negative)    , "negative"      },
		{ ValueDomain(Domain::not_negative), "not_negative"  },
		{ ValueDomain(Domain::positive)    , "positive"      },
		{ ValueDomain(Domain::not_positive), "not_positive"  },
	});


	constexpr auto make_multi_names = []() {
		constexpr std::size_t variadic_name_max = std::max_element(fn::variadic_props_table.begin(), fn::variadic_props_table.end(),
			[](const auto& fst, const auto& snd) { return fst.name.size() < snd.name.size(); })->name.size();

		std::array<std::array<char, variadic_name_max + 4u>, (unsigned)Variadic::COUNT> multi_names = {};
		for (std::size_t i = 0; i < multi_names.size(); i++) {
			std::size_t j = 0;
			const std::string_view src_name = fn::variadic_props_table.at(i).name;
			for (; j < src_name.size(); j++) {
				multi_names.at(i).at(j) = src_name.at(j);
			}
			//extend length by four for this end:
			multi_names.at(i).at(j++) = '.';
			multi_names.at(i).at(j++) = '.';
			multi_names.at(i).at(j++) = '.';
		}

		return multi_names;
	};
	constexpr auto multi_names = make_multi_names();

	constexpr std::string_view name_of(const PnVariablesType r) noexcept
	{
		if (r.is<Multi>()) {
			return multi_names[(unsigned)r.to<Multi>()].data();
		}
		return find(type_table, &TypeProps::type, r).name;
	}

	constexpr PnVariablesType type_of(const std::string_view s) noexcept
	{
		const auto multi = std::find_if(multi_names.begin(), multi_names.end(),
			[s](const auto& arr) { return std::string_view(arr.data()) == s; });
		if (multi != multi_names.end()) {
			return Multi((unsigned)std::distance(multi_names.begin(), multi));
		}
		return search(type_table, &TypeProps::name, s).type;
	}

    
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
			static constexpr MathIdx build(S& store, const std::uint32_t match_data_idx, const TreeMatchOwning restr)
			{
				const std::array<MathIdx, 2> parameters = {
					build_value(store, Complex{ static_cast<double>(match_data_idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(restr)), MathType(Literal::symbol)),
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
						assert(in_domain(*new_ref.new_at(params[0]), Domain::natural_0));
						assert(params[1].get_type() == Literal::symbol);
						assert(type_of(new_ref.new_at(params[1])->characters).is<TreeMatchOwning>());
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

			constexpr TreeMatchOwning restr() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[1])->characters).to<Restriction>();
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
			static constexpr MathIdx build(S& store, const std::uint32_t idx, const Multi type)
			{
				const std::array<MathIdx, 2> parameters = {
					build_value(store, Complex{ static_cast<double>(idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(type)), MathType(Literal::symbol)),
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
						assert(in_domain(*new_ref.new_at(params[0]), Domain::natural_0));
						assert(params[1].get_type() == Literal::symbol);
						assert(type_of(new_ref.new_at(params[1])->characters).is<Multi>());
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

			constexpr Multi type() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[1])->characters).to<Multi>();
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
						assert(in_domain(*new_ref.new_at(params[0]), Domain::natural_0));
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
		//  .domain as variable (same name as calling name_of(.restr)) in third parameter
		class IntermediateValueMatch
		{
			UnsaveRef ref;

			constexpr IntermediateValueMatch(const UnsaveRef new_ref) noexcept :ref(new_ref) {}

		public:
			static constexpr std::string_view function_name = "_VM";

			template<StoreLike S>
			static constexpr MathIdx build(S& store, const std::uint32_t match_data_idx, const ValueDomain domain)
			{
				const std::array<MathIdx, 3> parameters = {
					IntermediateValueProxy::build(store, match_data_idx),
					build_value(store, Complex{ static_cast<double>(match_data_idx), 0.0 }),
					MathIdx(CharVector::build(store, name_of(domain)), MathType(Literal::symbol)),
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
						assert(in_domain(*new_ref.new_at(params[1]), Domain::natural_0));
						assert(params[2].get_type() == Literal::symbol);
						assert(type_of(new_ref.new_at(params[2])->characters).is<ValueDomain>());
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

			constexpr ValueDomain domain() const noexcept
			{
				const IndexVector& params = *this->ref;
				return type_of(this->ref.new_at(params[2])->characters).to<ValueDomain>();
			}
		}; //class IntermediateValueMatch
	} //namespace math_rep



	struct [[nodiscard]] PatternParts
	{
		ParseView declarations;
		ParseView lhs;
		ParseView rhs;

		//input is assumed to be of form "<declarations> | <lhs> = <rhs>"
		//or, if no MatchVariables occur, of form "<lhs> = <rhs>"
		PatternParts(const ParseView input);
	};

	//data belonging to one TreeMatchVariable relevant while constructing pattern
	struct [[nodiscard]] TreeNameLookup
	{
		std::string_view name;
		TreeMatchOwning restr;
		StupidBufferVector<MathIdx, 4u> lhs_instances;
		StupidBufferVector<MathIdx, 4u> rhs_instances;

		TreeNameLookup(std::string_view new_name, TreeMatchOwning new_restr) noexcept
			:name(new_name), restr(new_restr) {}
	};

	//data belonging to one ValueMatchVariable relevant while constructing pattern
	struct [[nodiscard]] ValueNameLookup
	{
		std::string_view name;
		ValueDomain domain;
		StupidBufferVector<MathIdx, 4u> lhs_instances;
		StupidBufferVector<MathIdx, 4u> rhs_instances;

		ValueNameLookup(std::string_view new_name, Domain new_form) noexcept
			:name(new_name), domain(new_form) {}
	};

	struct [[nodiscard]] MultiNameLookup
	{
		std::string_view name;
		std::size_t lhs_count; //(only used to throw error if multiple instances exist in lhs)
		std::size_t rhs_count; //(only used to throw error if multiple instances exist in rhs)
		Multi type;

		MultiNameLookup(std::string_view new_name, const Multi new_type) noexcept
			:name(new_name), lhs_count(0u), rhs_count(0u), type(new_type) {}
	};

	//only exists during construction of pattern
	struct NameLookupTable
	{
		std::vector<TreeNameLookup> tree_table;
		std::vector<ValueNameLookup> value_table;
		std::vector<MultiNameLookup> multi_table;
		bool build_lhs = true; //false -> currently building rhs

		//assumes to only get declarations part of pattern
		NameLookupTable(ParseView declarations);

		MathIdx insert_instance(MathStore& store, const ParseView input);
	};

	struct PatternBuildFunction
	{
		//the index of name in table is also match_data_idx of TreeMatchVariable
		NameLookupTable& table;

		//equivalent to build() for pattern
		MathIdx operator()(MathStore& store, ParseView input);
	};


} //namespace bmath::intern::pattern

namespace bmath::intern::print {

	void append_to_string(const pattern::UnsavePnRef ref, std::string& str, const int depth = 0);

	void appent_to_simple_tree(const pattern::UnsavePnRef ref, std::string& str, const int depth = 0);

} //namespace bmath::intern::print