#ifndef COOL_TREE_H
#define COOL_TREE_H
//////////////////////////////////////////////////////////
//
// file: cool-tree.h
//
// This file defines classes for each phylum and constructor
//
//////////////////////////////////////////////////////////

#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
#include "tac.h"
#include <map>
#include <iostream>
#include <vector>

// cool-tree.handcode.h
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class CgenNode;
// end of cool-tree.handcode.h

// define the class for phylum
// define simple phylum - Program
typedef class Program_class *Program;

class Program_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Program(); }
   virtual Program copy_Program() = 0;

   virtual void cgen(ostream&) = 0;		
   virtual void dump_with_types(ostream&, int) = 0; 
};


// define simple phylum - Class_
typedef class Class__class *Class_;

class Class__class : public tree_node {
public:
   tree_node *copy()		 { return copy_Class_(); }
   virtual Class_ copy_Class_() = 0;

   virtual Symbol get_name() = 0;			
   virtual Symbol get_filename() = 0;      
   virtual void dump_with_types(ostream&,int) = 0;
};


// define simple phylum - Feature
typedef class Feature_class *Feature;

class Feature_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Feature(); }
   virtual Feature copy_Feature() = 0;

   virtual void dump_with_types(ostream&,int) = 0;	
   virtual bool is_attr() = 0; 
};


// define simple phylum - Formal
typedef class Formal_class *Formal;

class Formal_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Formal(); }
   virtual Formal copy_Formal() = 0;

   virtual void dump_with_types(ostream&,int) = 0; 
   virtual Symbol get_name() = 0;	
   virtual Symbol get_type() = 0;
};

// define simple phylum - Expression
typedef class Expression_class *Expression;

class Expression_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Expression(); }
   virtual Expression copy_Expression() = 0;

   Symbol type;                                 
   Symbol get_type() { return type; }           
   Expression set_type(Symbol s) { type = s; return this; } 
   virtual void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int) = 0; 
   virtual void dump_with_types(ostream&,int) = 0;  
   void dump_type(ostream&, int);               
   Expression_class() { type = (Symbol) NULL; }	

   // IR generation interface
   virtual void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) = 0;

   // TODO: Decide if it makes sense to do this refactor, I think it just depends where I am going to be storing my IR
   // virtual void code_asm_from_ir(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int) = 0;
};


// define simple phylum - Case
typedef class Case_class *Case;

class Case_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Case(); }
   virtual Case copy_Case() = 0;

   virtual void dump_with_types(ostream& ,int) = 0;
};


// define the class for phylum - LIST
// define list phlyum - Classes
typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;


// define list phlyum - Features
typedef list_node<Feature> Features_class;
typedef Features_class *Features;


// define list phlyum - Formals
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;


// define list phlyum - Expressions
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;


// define list phlyum - Cases
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;


// define the class for constructors
// define constructor - program
class program_class : public Program_class {
public:
   Classes classes;
public:
   program_class(Classes a1) {
      classes = a1;
   }
   Program copy_Program();
   void dump(ostream& stream, int n);

   void cgen(ostream&);     			
   void dump_with_types(ostream&, int);  
};


// define constructor - class_
class class__class : public Class__class {
public:
   Symbol name;
   Symbol parent;
   Features features;
   Symbol filename;
public:
   class__class(Symbol a1, Symbol a2, Features a3, Symbol a4) {
      name = a1;
      parent = a2;
      features = a3;
      filename = a4;
   }
   Class_ copy_Class_();
   void dump(ostream& stream, int n);

   Symbol get_name()   { return name; }
   Symbol get_filename() { return filename; }
   void dump_with_types(ostream&,int);	
};


// define constructor - method
class method_class : public Feature_class {
public:
   Symbol name;
   Formals formals;
   Symbol return_type;
   Expression expr;
public:
   method_class(Symbol a1, Formals a2, Symbol a3, Expression a4) {
      name = a1;
      formals = a2;
      return_type = a3;
      expr = a4;
   }
   Feature copy_Feature();
   void dump(ostream& stream, int n);

   void dump_with_types(ostream&,int);
   bool is_attr() { return false; };
};


// define constructor - attr
class attr_class : public Feature_class {
public:
   Symbol name;
   Symbol type_decl;
   Expression init;
public:
   attr_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      init = a3;
   }
   Feature copy_Feature();
   void dump(ostream& stream, int n);

   void dump_with_types(ostream&,int);
   bool is_attr() { return true; } 
};


// define constructor - formal
class formal_class : public Formal_class {
public:
   Symbol name;
   Symbol type_decl;
public:
   formal_class(Symbol a1, Symbol a2) {
      name = a1;
      type_decl = a2;
   }
   Formal copy_Formal();
   void dump(ostream& stream, int n);

   void dump_with_types(ostream&,int);
   Symbol get_name() { return name; } 
   Symbol get_type() { return type_decl; } 
};


// define constructor - branch
class branch_class : public Case_class {
public:
   Symbol name;
   Symbol type_decl;
   Expression expr;
public:
   branch_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      expr = a3;
   }
   Case copy_Case();
   void dump(ostream& stream, int n);

   void dump_with_types(ostream& ,int);
};


// define constructor - assign
class assign_class : public Expression_class {
public:
   Symbol name;
   Expression expr;
public:
   assign_class(Symbol a1, Expression a2) {
      name = a1;
      expr = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - static_dispatch
class static_dispatch_class : public Expression_class {
public:
   Expression expr;
   Symbol type_name;
   Symbol name;
   Expressions actual;
public:
   static_dispatch_class(Expression a1, Symbol a2, Symbol a3, Expressions a4) {
      expr = a1;
      type_name = a2;
      name = a3;
      actual = a4;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - dispatch
class dispatch_class : public Expression_class {
public:
   Expression expr;
   Symbol name;
   Expressions actual;
public:
   dispatch_class(Expression a1, Symbol a2, Expressions a3) {
      expr = a1;
      name = a2;
      actual = a3;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - cond
class cond_class : public Expression_class {
public:
   Expression pred;
   Expression then_exp;
   Expression else_exp;
public:
   cond_class(Expression a1, Expression a2, Expression a3) {
      pred = a1;
      then_exp = a2;
      else_exp = a3;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - loop
class loop_class : public Expression_class {
public:
   Expression pred;
   Expression body;
public:
   loop_class(Expression a1, Expression a2) {
      pred = a1;
      body = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - typcase
class typcase_class : public Expression_class {
public:
   Expression expr;
   Cases cases;
public:
   typcase_class(Expression a1, Cases a2) {
      expr = a1;
      cases = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - block
class block_class : public Expression_class {
public:
   Expressions body;
public:
   block_class(Expressions a1) {
      body = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - let
class let_class : public Expression_class {
public:
   Symbol identifier;
   Symbol type_decl;
   Expression init;
   Expression body;
public:
   let_class(Symbol a1, Symbol a2, Expression a3, Expression a4) {
      identifier = a1;
      type_decl = a2;
      init = a3;
      body = a4;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - plus
class plus_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   plus_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - sub
class sub_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   sub_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - mul
class mul_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   mul_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - divide
class divide_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   divide_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - neg
class neg_class : public Expression_class {
public:
   Expression e1;
public:
   neg_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - lt
class lt_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   lt_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - eq
class eq_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   eq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - leq
class leq_class : public Expression_class {
public:
   Expression e1;
   Expression e2;
public:
   leq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - comp
class comp_class : public Expression_class {
public:
   Expression e1;
public:
   comp_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - int_const
class int_const_class : public Expression_class {
public:
   Symbol token;
public:
   int_const_class(Symbol a1) {
      token = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - bool_const
class bool_const_class : public Expression_class {
public:
   Boolean val;
public:
   bool_const_class(Boolean a1) {
      val = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - string_const
class string_const_class : public Expression_class {
public:
   Symbol token;
public:
   string_const_class(Symbol a1) {
      token = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - new_
class new__class : public Expression_class {
public:
   Symbol type_name;
public:
   new__class(Symbol a1) {
      type_name = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - isvoid
class isvoid_class : public Expression_class {
public:
   Expression e1;
public:
   isvoid_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - no_expr
class no_expr_class : public Expression_class {
public:
public:
   no_expr_class() {
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};

// define constructor - object
class object_class : public Expression_class {
public:
   Symbol name;
public:
   object_class(Symbol a1) {
      name = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

   void code(ostream&, CgenNode*, SymbolTable<std::string, int>&, int&, int);
   void dump_with_types(ostream&,int);

   // IR generation interface
   void code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter);
};


// define the prototypes of the interface
Classes nil_Classes();
Classes single_Classes(Class_);
Classes append_Classes(Classes, Classes);
Features nil_Features();
Features single_Features(Feature);
Features append_Features(Features, Features);
Formals nil_Formals();
Formals single_Formals(Formal);
Formals append_Formals(Formals, Formals);
Expressions nil_Expressions();
Expressions single_Expressions(Expression);
Expressions append_Expressions(Expressions, Expressions);
Cases nil_Cases();
Cases single_Cases(Case);
Cases append_Cases(Cases, Cases);
Program program(Classes);
Class_ class_(Symbol, Symbol, Features, Symbol);
Feature method(Symbol, Formals, Symbol, Expression);
Feature attr(Symbol, Symbol, Expression);
Formal formal(Symbol, Symbol);
Case branch(Symbol, Symbol, Expression);
Expression assign(Symbol, Expression);
Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
Expression dispatch(Expression, Symbol, Expressions);
Expression cond(Expression, Expression, Expression);
Expression loop(Expression, Expression);
Expression typcase(Expression, Cases);
Expression block(Expressions);
Expression let(Symbol, Symbol, Expression, Expression);
Expression plus(Expression, Expression);
Expression sub(Expression, Expression);
Expression mul(Expression, Expression);
Expression divide(Expression, Expression);
Expression neg(Expression);
Expression lt(Expression, Expression);
Expression eq(Expression, Expression);
Expression leq(Expression, Expression);
Expression comp(Expression);
Expression int_const(Symbol);
Expression bool_const(Boolean);
Expression string_const(Symbol);
Expression new_(Symbol);
Expression isvoid(Expression);
Expression no_expr();
Expression object(Symbol);

#endif
