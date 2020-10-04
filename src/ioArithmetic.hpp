#pragma once

#include "parseTerm.hpp"
#include "arithmeticTerm.hpp"

namespace bmath::intern::arithmetic {

	//returns position of first non_arithmetic token in view
	std::size_t find_first_not_arithmetic(const TokenView view);

	struct Head
	{
		std::size_t where;
		enum class Type
		{
			sum,	     //where specifies position of found operator
			negate,	     //where specifies position of found operator
			product,     //where specifies position of found operator
			power,       //where specifies position of found operator
			value,       //where is unspecidied
			variable,    //where is unspecified
			group,       //where is unspecified
			function,    //where specifies position of opening parenthesis
		} type;
	};

	//decides what type the outhermost element has
	//offset is used to determine error position relative to begin of whole term
	Head find_head_type(const TokenView token_view, std::size_t offset);

	//utility for build
	TypedIdx build_value(Store& store, double re, double im = 0.0);

	//returns head, offset is used to determine error position relative begin of whole term
	TypedIdx build(Store& store, ParseView view);

	//VariadicTraits must include:
	//unsing declaration Object_T: type to construct (e.g. Sum)
	//<Enum type> type_name: name of operation in enum representing all types in store
	//double neutral_element: the value not changing any other value if both are combined using operation e.g. 0.0
	//char operator_char: the character symbolizing the operation, e.g. '+'
	//char inverse_operator_char: the character symbolizing the inverse operation, e.g. '-'
	//Token operator_token: the token symbolizing both normal and inverse operation, e.g. token::sum

	//BuildInverse recieves an already build term (by TypedIdx_T) and returns the inverse (by TypedIdx_T)
	//  e.g. for sum, it should turn "a" -> "a*(-1)", for product "a" -> "a^(-1)"
	//BuildAny can buld any type of term, this function will very likely already call build_variadic.
	template<typename VariadicTraits, typename TypedIdx_T, typename TermStore_T,
		typename BuildInverse, typename BuildAny>
	TypedIdx_T build_variadic(TermStore_T& store, ParseView input, std::size_t op_idx,
			BuildInverse build_inverse, BuildAny build_any);

	TypedIdx build_function(Store& store, ParseView input, const std::size_t open_par);

	namespace print {

		enum class PrintExtras { pow, COUNT };
		using PrintType = CombinedEnum<Type, Type::COUNT, PrintExtras, PrintExtras::COUNT>;

		//operator precedence (used to decide if parentheses are nessecary in out string)
		constexpr auto infixr_table = std::to_array<std::pair<PrintType, int>>({
			{ Type::known_function,   0 },
			{ Type::generic_function, 0 },
			{ Type::sum,      	      2	},
			{ Type::product,          4 },
			{ PrintExtras::pow,       5 },
			{ Type::variable,         6 },
			{ Type::complex,          6 },//may be printed as sum/product itself, then (maybe) has to add parentheses on its own
		});
		constexpr int infixr(PrintType type) { return find(infixr_table, type); }

		void append_complex(const std::complex<double> val, std::string& dest, int parent_operator_precedence);

		void append_real(double val, std::string& dest);

		std::optional<double> get_negative_real(const Store& store, const TypedIdx ref);

		//returns base, if ref is actually <base>^(-1)
		std::optional<TypedIdx> get_pow_neg1(const Store& store, const TypedIdx ref);

		struct GetNegativeProductResult { double negative_factor; std::vector<TypedIdx> other_factors; };
		std::optional<GetNegativeProductResult> get_negative_product(const Store& store, const TypedIdx ref);

		void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_infixr = 0);

		//prettier, but also slower
		std::string to_pretty_string(const Store& store, const TypedIdx ref, const int parent_infixr = 0);

		void to_memory_layout(const Store& store, const TypedIdx ref, std::vector<std::string>& content);

	} //namespace print

} //namespace bmath::intern::arithmetic