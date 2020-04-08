#pragma once	//NOLINT

#include "baseTerm.hpp"

namespace bmath::intern {

	//Bool_Term is meant as helper for Pattern_Term, as it enables to restrict relations between Pattern_Variables.
	class Bool_Term
	{
	public:
		virtual ~Bool_Term() {} //all memmbers are held in derived classes -> nothing to do here

		virtual bool evaluate() const = 0;
	};

	class And final : public Bool_Term
	{
	public:
		Bool_Term* fst;
		Bool_Term* snd;

		And(Bool_Term* fst_, Bool_Term* snd_) :fst(fst_), snd(snd_) {}
		virtual ~And() { delete fst; delete snd; }

		virtual bool evaluate() const override { return fst->evaluate() && snd->evaluate(); }
	};

	class Or final : public Bool_Term
	{
	public:
		Bool_Term* fst;
		Bool_Term* snd;

		Or(Bool_Term* fst_, Bool_Term* snd_) :fst(fst_), snd(snd_) {}
		virtual ~Or() { delete fst; delete snd; }

		virtual bool evaluate() const override { return fst->evaluate() || snd->evaluate(); }
	};

	class Not final : public Bool_Term
	{
	public:
		Bool_Term* arg;

		Not(Bool_Term* arg_) : arg(arg_) {}
		virtual ~Not() { delete arg; }

		virtual bool evaluate() const override { return !arg->evaluate(); }
	};

	class Equivalent final : public Bool_Term
	{
	public:
		Bool_Term* fst;
		Bool_Term* snd;

		Equivalent(Bool_Term* fst_, Bool_Term* snd_) :fst(fst_), snd(snd_) {}
		virtual ~Equivalent() { delete fst; delete snd; }

		virtual bool evaluate() const override { return fst->evaluate() == snd->evaluate(); }
	};

	class Fst_Implicates_Snd final : public Bool_Term
	{
	public:
		Bool_Term* fst;
		Bool_Term* snd;

		Fst_Implicates_Snd(Bool_Term* fst_, Bool_Term* snd_) :fst(fst_), snd(snd_) {}
		virtual ~Fst_Implicates_Snd() { delete fst; delete snd; }

		virtual bool evaluate() const override { return fst->evaluate() ? snd->evaluate() : true; }
	};

	class Equals final : public Bool_Term
	{
	public:
		Pattern_Term* fst;
		Pattern_Term* snd;

		Equals(Pattern_Term* fst_, Pattern_Term* snd_) :fst(fst_), snd(snd_) {}
		virtual ~Equals() { delete fst; delete snd; }

		virtual bool evaluate() const override { return /*fst->equals(*snd);*/ false; }
	};
}