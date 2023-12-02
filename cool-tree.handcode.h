//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
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

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void cgen(ostream&) = 0;		\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void cgen(ostream&);     			\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_name() = 0;  	\
virtual Symbol get_parent() = 0;    	\
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0;	\
virtual Features get_features() = 0;


#define class__EXTRAS                                  \
Symbol get_name()   { return name; }		       \
Symbol get_parent() { return parent; }     	       \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);	\
Features get_features() { return features; }	                 


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;	\
virtual bool is_attr() = 0; \
virtual Expression get_expression() = 0; \
virtual Symbol get_name() = 0;

#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);

#define attr_EXTRAS \
bool is_attr() { return true; } \
Expression get_expression() { return init; }	\
Symbol get_declared_type() { return type_decl; }	\
Symbol get_name() { return name; }

#define method_EXTRAS \
bool is_attr() { return false; }	\
Expression get_expression() { return expr; }	\
Symbol get_name() { return name; } \
Formals get_parameters() { return formals; }

#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;	\


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);	\


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void code(ostream&, CgenNode*) = 0; \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }	\

#define Expression_SHARED_EXTRAS           \
void code(ostream&, CgenNode*); 			   \
void dump_with_types(ostream&,int); 

#define object_EXTRAS                                   \
Symbol get_name() { return name; }

#define plus_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define sub_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define mul_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define divide_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define neg_EXTRAS	\
Expression get_rhs() { return e1; }

#define lt_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define eq_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define leq_EXTRAS	\
Expression get_lhs() { return e1; } \
Expression get_rhs() { return e2; }

#define comp_EXTRAS	\
Expression get_rhs() { return e1; }

#define isvoid_EXTRAS	\
Expression get_rhs() { return e1; }

#define new__EXTRAS	\
Symbol get_type_name() { return type_name; }

#define block_EXTRAS	\
Expressions get_body() { return body; };

#define loop_EXTRAS	\
Expression get_pred() { return pred; }	\
Expression get_body() { return body; }

#define assign_EXTRAS	\
Symbol get_symbol_name() { return name; }	\
Expression get_expr() { return expr; }


#endif
