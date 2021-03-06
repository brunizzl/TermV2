#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"
#include "pattern.hpp"

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
	[[nodiscard]] MathIdx build(MathStore& store, ParseView view);


	template<StoreLike Store_T>
	[[nodiscard]] MathIdx build_value(Store_T& store, const std::complex<double> complex) noexcept
	{
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) MathUnion(complex);
		return MathIdx(result_idx, Literal::complex);
	}

	template<StoreLike Store_T>
	[[nodiscard]] MathIdx build_negated(Store_T& store, const MathIdx to_negate) noexcept
	{
		const MathIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) MathUnion(IndexVector({ minus_1, to_negate }));
		return MathIdx(result_idx, Comm::product);
	}

	template<StoreLike Store_T>
	[[nodiscard]] MathIdx build_inverted(Store_T& store, const MathIdx to_invert) noexcept
	{
		const MathIdx minus_1 = build_value(store, -1.0);
		const std::size_t result_idx = store.allocate_one();
		new (&store.at(result_idx)) MathUnion(IndexVector({ to_invert, minus_1 }));
		return MathIdx(result_idx, Fn::pow);
	}

	struct SumTraits
	{
		static constexpr MathType type_name = MathType(Comm::sum);
		static constexpr char operator_char = '+';
		static constexpr char inverse_operator_char = '-';
		static constexpr Token operator_token = token::sum;
	};

	struct ProductTraits
	{
		static constexpr MathType type_name = MathType(Comm::product);
		static constexpr char operator_char = '*';
		static constexpr char inverse_operator_char = '/';
		static constexpr Token operator_token = token::product;
	};

	//VariadicTraits must include:
	//<Enum type> type_name: name of operation in enum representing all types in store
	//char operator_char: the character symbolizing the operation, e.g. '+'
	//char inverse_operator_char: the character symbolizing the inverse operation, e.g. '-'
	//Token operator_token: the token symbolizing both normal and inverse operation, e.g. token::sum

	//BuildInverse recieves an already build term (by TypedIdx) and returns the inverse (by TypedIdx)
	//  e.g. for sum, it should turn "a" -> "a*(-1)", for product "a" -> "a^(-1)"
	//BuildAny can build any type of term from a ParseView, this function will very likely already call build_variadic.
	template<typename VariadicTraits, StoreLike Store_T, typename BuildInverse, typename BuildAny>
	MathIdx build_variadic(Store_T& store, ParseView input, std::size_t op_idx, BuildInverse build_inverse, BuildAny build_any)
	{
		StupidBufferVector<MathIdx, 16> result_buffer;
		{
			const ParseView subterm_view = input.steal_prefix(op_idx);
			result_buffer.push_back(build_any(store, subterm_view));
		}
		while (input.size()) {
			const char current_operator = input.chars[0u];
			input.remove_prefix(1u); //remove current_operator;
			op_idx = find_first_of_skip_pars(input.tokens, VariadicTraits::operator_token);
			const ParseView subterm_view = input.steal_prefix(op_idx);
			const MathIdx subterm = build_any(store, subterm_view);
			switch (current_operator) {
			case VariadicTraits::operator_char:
				result_buffer.push_back(subterm);
				break;
			case VariadicTraits::inverse_operator_char:
				result_buffer.push_back(build_inverse(store, subterm));
				break;
			default: assert(false);
			}
		}
		return MathIdx(IndexVector::build(store, result_buffer), VariadicTraits::type_name);
	} //build_variadic

	template<StoreLike Store_T, typename BuildAny>
	[[nodiscard]] MathIdx build_function(Store_T& store, ParseView input, const std::size_t open_par, BuildAny build_any)
	{
		const std::string_view name = input.to_string_view(0u, open_par);

		StupidBufferVector<MathIdx, 12> result_parameters;
		input.remove_suffix(1u);
		input.remove_prefix(open_par + 1u);	//only arguments are left
		if (input.size()) [[likely]] { //else no parameters at all
			const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
			const auto param_view = input.steal_prefix(comma); //now input starts with comma
			result_parameters.push_back(build_any(store, param_view));
		}
			while (input.size()) {
				input.remove_prefix(1u); //erase leading comma
				const std::size_t comma = find_first_of_skip_pars(input.tokens, token::comma);
				const auto param_view = input.steal_prefix(comma);
				result_parameters.push_back(build_any(store, param_view));
			}

		if (const Fn type = fn::fn_type_of(name); type != Fn::COUNT) {
			if (result_parameters.size() != fn::arity(type)) [[unlikely]] throw ParseFailure{ input.offset, 
				"wrong numer of function parameters" };
			return MathIdx(IndexVector::build(store, result_parameters), type);
		}
		else if (const Variadic type = fn::variadic_type_of(name); type != Variadic(Variadic::COUNT)) {
			return MathIdx(IndexVector::build(store, result_parameters), type);
		}
		else { //build NamedFn
			return fn::build_named_fn<MathType>(store, name, result_parameters);
		}
	} //build_function




	namespace print {

		void append_complex(const std::complex<double> val, std::string& dest, int parent_infixr);

		void append_to_string(const UnsaveRef ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const UnsaveRef ref, const int parent_infixr = 0);

		std::string to_memory_layout(const MathStore& store, const std::initializer_list<const MathIdx> heads);

		//returns tree representation of ref
		//offset specifies how far the whole tree is shifted to the right
		std::string to_tree(const UnsaveRef ref, const std::size_t offset = 0u);

	} //namespace print

} //namespace bmath::intern