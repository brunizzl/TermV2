#pragma once

#include <complex>
#include <span>
#include <optional>
#include <compare>

#include "typedIndex.hpp"
#include "termStore.hpp"
#include "termColony.hpp"
#include "parseTerm.hpp"

namespace bmath::intern::arithmetic {

	enum class Type {
		sum,
		product,
		known_function,
		generic_function,
		variable,
		complex,
		COUNT	//has to be last element
	};

	//more unique (meaning harder to match) is smaller
	constexpr auto uniqueness_table = std::to_array<std::pair<Type, int>>({
		{ Type::generic_function, 00 }, //order of parameters is given -> most unique
		{ Type::known_function  , 16 }, //order of parameters is given -> most unique
		{ Type::product         , 32 }, //order of operands my vary -> second most unique
		{ Type::sum             , 48 }, //order of operands my vary -> second most unique
		{ Type::variable        , 64 }, //can only match pattern variable directly -> least unique
		{ Type::complex         , 80 }, //can only match pattern variable directly -> least unique
	});
	constexpr int uniqueness(Type type) noexcept { return find(uniqueness_table, type); }

	using TypedIdx = BasicTypedIdx<Type, Type::COUNT, std::uint32_t>;
	using TypedIdxColony = TermSLC<std::uint32_t, TypedIdx, 3>;

	using Sum            = TypedIdxColony;
	using Product        = TypedIdxColony;

	enum class FnType : std::uint32_t
	{
		asinh,	//params[0] := argument
		acosh,	//params[0] := argument
		atanh,	//params[0] := argument
		asin,	//params[0] := argument
		acos,	//params[0] := argument
		atan,	//params[0] := argument
		sinh,	//params[0] := argument
		cosh,	//params[0] := argument
		tanh,	//params[0] := argument
		sqrt,	//params[0] := argument
		pow,    //params[0] := base      params[1] := expo    
		log,	//params[0] := base      params[1] := argument
		_logn, //same as log, this enum may only occur during parsing
		exp,	//params[0] := argument
		sin,	//params[0] := argument
		cos,	//params[0] := argument
		tan,	//params[0] := argument
		abs,	//params[0] := argument
		arg,	//params[0] := argument
		ln,		//params[0] := argument
		re,		//params[0] := argument
		im,		//params[0] := argument
		UNKNOWN //has to be last element, doubles as count
	};

	struct KnownFunction
	{
		FnType type;

		//if any buildin funtion exeeds a parameter count of 3, a more involved structure needs to replace this.
		TypedIdx params[3];
	};

	struct GenericFunction
	{
		static constexpr std::size_t short_name_max = 4 + 7; //plus '\0' at end, supplied by name_size

		//if name_size == NameSize::small short_name is used, but as if would be of length 11
		//this is perhaps undefined behavior, but im feeling unsave today >:)	
		union
		{
			char short_name[4] = "";
			std::uint32_t long_name_idx;	//points to TermString128 containing name (if active)
		};
	private:
		char short_name_extension[7] = ""; //just implementation detail, no one needs to see this
	public:
		//if small is active, it doubles in purpose as '\0' character to end the name
		enum class NameSize :char { small = '\0', longer } name_size = NameSize::small;

		std::uint32_t params_idx = 0; //points to TypedIdxColony containing the parameters
	};

	using  Variable = TermString128;
	using Complex = std::complex<double>;

	union TypesUnion
	{
		KnownFunction known_function;
		GenericFunction generic_function;
		Complex complex;
		TermString128 string;	//Variable and GenericFunction may allocate additional string nodes
		TypedIdxColony index_slc; //representing Sum, Product or GenericFunction's extra parameters 
		Variable variable; //alias for string
		Sum sum;         //alias for index_slc
		Product product; //alias for index_slc

		TypesUnion() :complex() {}

		TypesUnion(const KnownFunction&   val) :known_function(val)   {}
		TypesUnion(const GenericFunction& val) :generic_function(val) {}
		TypesUnion(const Complex&         val) :complex(val)          {}
		TypesUnion(const TermString128&   val) :string(val)           {} 
		TypesUnion(const TypedIdxColony&  val) :index_slc(val)        {}
	};

	static_assert(sizeof(TypesUnion) * 8 == 128);
	using Store = TermStore<TypesUnion>;

	//utility for both KnownFunction and GenericFunction
	namespace fn {

		constexpr auto name_table = std::to_array<std::pair<FnType, std::string_view>>({
			{ FnType::asinh, { "asinh" } },	
			{ FnType::acosh, { "acosh" } },
			{ FnType::atanh, { "atanh" } },	
			{ FnType::asin , { "asin"  } },	
			{ FnType::acos , { "acos"  } },	
			{ FnType::atan , { "atan"  } },	
			{ FnType::sinh , { "sinh"  } },	
			{ FnType::cosh , { "cosh"  } },	
			{ FnType::tanh , { "tanh"  } },	
			{ FnType::sqrt , { "sqrt"  } },	
			{ FnType::pow  , { "pow"   } },   
			{ FnType::log  , { "log"   } },	
			{ FnType::exp  , { "exp"   } },	
			{ FnType::sin  , { "sin"   } },	
			{ FnType::cos  , { "cos"   } },	
			{ FnType::tan  , { "tan"   } },	
			{ FnType::abs  , { "abs"   } },	
			{ FnType::arg  , { "arg"   } },	
			{ FnType::ln   , { "ln"    } },	
			{ FnType::re   , { "re"    } },	
			{ FnType::im   , { "im"    } },	
		});
		constexpr std::string_view name_of(FnType type) noexcept 
		{ return find(name_table, type); }

		constexpr auto type_table = std::to_array<std::pair<std::string_view, FnType>>({
			{ { "asinh" }, FnType::asinh },	
			{ { "acosh" }, FnType::acosh },
			{ { "atanh" }, FnType::atanh },	
			{ { "asin"  }, FnType::asin  },	
			{ { "acos"  }, FnType::acos  },	
			{ { "atan"  }, FnType::atan  },	
			{ { "sinh"  }, FnType::sinh  },	
			{ { "cosh"  }, FnType::cosh  },	
			{ { "tanh"  }, FnType::tanh  },	
			{ { "sqrt"  }, FnType::sqrt  },	
			{ { "pow"   }, FnType::pow   },   
			{ { "log"   }, FnType::log   },	
			{ { "exp"   }, FnType::exp   },	
			{ { "sin"   }, FnType::sin   },	
			{ { "cos"   }, FnType::cos   },	
			{ { "tan"   }, FnType::tan   },	
			{ { "abs"   }, FnType::abs   },	
			{ { "arg"   }, FnType::arg   },		
			{ { "loge"  }, FnType::ln    },	
			{ { "ln"    }, FnType::ln    },	
			{ { "re"    }, FnType::re    },	
			{ { "im"    }, FnType::im    },	
		});

		constexpr auto param_count_table = std::to_array<std::pair<FnType, std::size_t>>({
			{ FnType::asinh, 1 },	
			{ FnType::acosh, 1 },
			{ FnType::atanh, 1 },	
			{ FnType::asin , 1 },	
			{ FnType::acos , 1 },	
			{ FnType::atan , 1 },	
			{ FnType::sinh , 1 },	
			{ FnType::cosh , 1 },	
			{ FnType::tanh , 1 },	
			{ FnType::sqrt , 1 },	
			{ FnType::pow  , 2 }, //<- only for these fuckers >:(  
			{ FnType::log  , 2 },	//<- only for these fuckers >:(
			{ FnType::exp  , 1 },	
			{ FnType::sin  , 1 },	
			{ FnType::cos  , 1 },	
			{ FnType::tan  , 1 },	
			{ FnType::abs  , 1 },	
			{ FnType::arg  , 1 },	
			{ FnType::ln   , 1 },	
			{ FnType::re   , 1 },	
			{ FnType::im   , 1 },	
		});
		constexpr std::size_t param_count(FnType type) noexcept 
		{ return find(param_count_table, type); }

		Complex eval(FnType type, const std::array<Complex, 3>& params);

		//appends only name, no parentheses or anything fancy
		void append_name(const Store& store, const GenericFunction& func, std::string& str);

		std::strong_ordering compare_name(const Store& store,
			const GenericFunction& func_1, const GenericFunction& func_2);

		//only expects actual name part of function, e.g. "asin", NOT "asin(...)"
		//if name is one of FnType, that is returned, else FnType::UNKNOWN
		FnType type_of(const ParseView input) noexcept;

		inline std::span<TypedIdx> range(KnownFunction& func) noexcept
		{ return { func.params, param_count(func.type) }; }

		inline std::span<const TypedIdx> range(const KnownFunction& func) noexcept
		{ return { func.params, param_count(func.type) }; }

		inline auto range(Store& store, GenericFunction& func) noexcept 
		{ return TypedIdxColony::SLCRange<Store>(store, func.params_idx); }

		inline auto range(const Store& store, const GenericFunction& func) noexcept 
		{ return TypedIdxColony::SLCRange<const Store>(store, func.params_idx); }

	} //namespace fn

	//utility for variadic types (Sum and Product)
	namespace vdc {

		inline auto range(Store& store, std::uint32_t vd_idx) noexcept
		{ return TypedIdxColony::SLCRange<Store>(store, vd_idx); }

		inline auto range(const Store& store, std::uint32_t vd_idx) noexcept 
		{ return TypedIdxColony::SLCRange<const Store>(store, vd_idx); }

		struct SumTraits
		{
			using Object_T = Sum;
			static constexpr Type type_name = Type::sum;
			static constexpr double neutral_element = 0.0;
			static constexpr char operator_char = '+';
			static constexpr char inverse_operator_char = '-';
			static constexpr Token operator_token = token::sum;
		};

		struct ProductTraits
		{
			using Object_T = Product;
			static constexpr Type type_name = Type::product;
			static constexpr double neutral_element = 1.0;
			static constexpr char operator_char = '*';
			static constexpr char inverse_operator_char = '/';
			static constexpr Token operator_token = token::product;
		};

	} //namespace vdc

	//removes subtree starting at ref from store
	void free_tree(Store& store, const TypedIdx ref);

	//evaluates tree if possible, throws if variables of unknown value /generic_functions are present
	[[nodiscard]] Complex eval_tree(const Store& store, const TypedIdx ref);

	void append_to_string(const Store& store, const TypedIdx ref, std::string& str, const int parent_precedence = -1);

	void to_memory_layout(const Store& store, const TypedIdx ref, std::vector<std::string>& content);

	//flatten sums holding als summands and products holding products as factors
	void flatten_variadic(Store& store, const TypedIdx ref);

	//if a subtree can be fully evaluated, it will be, even if the result can not be stored exactly in 
	//floating point/ the computation is unexact
	//the evaluated subtree also deletes itself, meaning the caller needs to reinsert the value,
	//  if a value was returned
	[[nodiscard]] std::optional<Complex> combine_values_unexact(Store& store, const TypedIdx ref);

	//compares two subterms in same term, assumes both to have their variadic parts sorted
	std::strong_ordering compare(const Store& store, const TypedIdx ref_1, const TypedIdx ref_2);

	//sorts variadic parts by compare
	void sort(Store& store, const TypedIdx ref);

}	//namespace bmath::intern::arithmetic

namespace bmath {

	class ArithmeticTerm
	{
		intern::arithmetic::TypedIdx head;
		intern::arithmetic::Store store;

	public:
		ArithmeticTerm(std::string name);
		ArithmeticTerm() = default;

		void flatten_variadic() noexcept;
		void combine_values_unexact() noexcept;
		void sort() noexcept;

		std::string show_memory_layout() const noexcept;
		std::string to_string() const noexcept;
	};	//class ArithmeticTerm

}	//namespace bmath



