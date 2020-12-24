#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"

namespace bmath::intern {

	struct Head
	{
		std::size_t where;
		enum class Type
		{
			sum,	     //where specifies position of found operator
			negate,	     //where specifies position of found operator
			product,     //where specifies position of found operator
			power,       //where specifies position of found operator
			real_value,  //where is unspecified
			imag_value,  //where is unspecified
			variable,    //where is unspecified
			group,       //where is unspecified
			function,    //where specifies position of opening parenthesis
			natural_computable,  //where is unspecified
			complex_computable,  //where is unspecified
		} type;
	};

	namespace compute {

		enum class Result { not_exactly_computable, natural, complex };

		//quite conservative in what operations are allowed, but still sometimes returns allowed if eval is unexact.
		// (happens if numbers are too large to be stored exactly)
		Result exactly_computable(const ParseView view) noexcept;

		//only expects operations '+', '*', '-' on numbers in { a + bi | a, b in Z }
		std::complex<double> eval_complex(ParseView view);

		//only expects operations '+', '*', '^' on numbers in N
		double eval_natural(ParseView view);

		//expects number in engineering notation
		double parse_value(const ParseView view);

	} //namespace compute

	//returns position of first non-arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view) noexcept;

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	[[nodiscard]] Head find_head_type(const ParseView view);

	//returns head
	[[nodiscard]] TypedIdx build(Store& store, ParseView view);


	template<typename Store_T>
	[[nodiscard]] TypedIdx build_value(Store_T& store, const std::complex<double> complex) noexcept
	{
		const std::size_t result_idx = store.allocate();
		new (&store.at(result_idx)) TypesUnion(complex);
		return TypedIdx(result_idx, Type(Leaf::complex));
	}

	template<typename Store_T>
	[[nodiscard]] TypedIdx build_negated(Store_T& store, const TypedIdx to_negate) noexcept
	{
		const TypedIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate();
		new (&store.at(result_idx)) TypesUnion(VariadicParams({ minus_1, to_negate }));
		return TypedIdx(result_idx, Type(Variadic::product));
	}

	template<typename Store_T>
	[[nodiscard]] TypedIdx build_inverted(Store_T& store, const TypedIdx to_invert) noexcept
	{
		const TypedIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate();
		new (&store.at(result_idx)) TypesUnion(FnParams({ to_invert, minus_1, TypedIdx(), TypedIdx() }));
		return TypedIdx(result_idx, Type(Fn::pow));
	}

	namespace pattern {

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
			pattern::Restriction restr;
			StupidBufferVector<TypedIdx, 4u> lhs_instances;
			StupidBufferVector<TypedIdx, 4u> rhs_instances;

			TreeNameLookup(std::string_view new_name, pattern::Restriction new_restr) noexcept
				:name(new_name), restr(new_restr) {}
		};
		
		//data belonging to one ValueMatchVariable relevant while constructing pattern
		struct [[nodiscard]] ValueNameLookup 
		{
			std::string_view name;
			Form form;
			StupidBufferVector<TypedIdx, 4u> lhs_instances;
			StupidBufferVector<TypedIdx, 4u> rhs_instances;

			ValueNameLookup(std::string_view new_name, Form new_form) noexcept
				:name(new_name), form(new_form) {}
		};

		struct [[nodiscard]] MultiNameLookup
		{
			std::string_view name;
			std::size_t lhs_count; //(only used to throw error if multiple instances exist in lhs)
			std::size_t rhs_count; //(only used to throw error if multiple instances exist in rhs)
			MultiPn type;

			MultiNameLookup(std::string_view new_name, const MultiPn new_type) noexcept 
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

			TypedIdx insert_instance(Store& store, const ParseView input);
		};

		struct PatternBuildFunction
		{
			//the index of name in table is also match_data_idx of TreeMatchVariable
			NameLookupTable& table;

			//equivalent to build() for pattern
			TypedIdx operator()(Store& store, ParseView input);
		};

	} //namespace pattern

	namespace print {

		void append_to_string(const Ref ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Ref ref, const int parent_infixr = 0);

		std::string to_memory_layout(const Store& store, const std::initializer_list<const TypedIdx> heads);

		//returns tree representation of ref
		//offset specifies how far the whole tree is shifted to the right
		std::string to_tree(const Ref ref, const std::size_t offset = 0u);

	} //namespace print

} //namespace bmath::intern