//////////////////////////////////////////////////////////
//
// file: cool-tree.cc
//
// This file defines the functions of each class
//
//////////////////////////////////////////////////////////


#include "tree.h"
#include "cool-tree.h"
#include "cgen.h"
#include "emit.h"
#include <sstream>
#include <algorithm>
#include <set>
#include "cgen_gc.h"
#include "tac.h"

extern Symbol
         arg,
         arg2,
         Bool,
         concat,
         cool_abort,
         copy,
         Int,
         in_int,
         in_string,
         IO,
         length,
         Main,
         main_meth,
         No_class,
         No_type,
         Object,
         out_int,
         out_string,
         prim_slot,
         self,
         SELF_TYPE,
         Str,
         str_field,
         substr,
         type_name,
         val;

int label_index  = 0;

// constructors' functions
Program program_class::copy_Program()
{
   return new program_class(classes->copy_list());
}


void program_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "program\n";
   classes->dump(stream, n+2);
}


Class_ class__class::copy_Class_()
{
   return new class__class(copy_Symbol(name), copy_Symbol(parent), features->copy_list(), copy_Symbol(filename));
}


void class__class::dump(ostream& stream, int n)
{
   stream << pad(n) << "class_\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, parent);
   features->dump(stream, n+2);
   dump_Symbol(stream, n+2, filename);
}


Feature method_class::copy_Feature()
{
   return new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression());
}


void method_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "method\n";
   dump_Symbol(stream, n+2, name);
   formals->dump(stream, n+2);
   dump_Symbol(stream, n+2, return_type);
   expr->dump(stream, n+2);
}


Feature attr_class::copy_Feature()
{
   return new attr_class(copy_Symbol(name), copy_Symbol(type_decl), init->copy_Expression());
}


void attr_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "attr\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, type_decl);
   init->dump(stream, n+2);
}


Formal formal_class::copy_Formal()
{
   return new formal_class(copy_Symbol(name), copy_Symbol(type_decl));
}


void formal_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "formal\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, type_decl);
}


Case branch_class::copy_Case()
{
   return new branch_class(copy_Symbol(name), copy_Symbol(type_decl), expr->copy_Expression());
}


void branch_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "branch\n";
   dump_Symbol(stream, n+2, name);
   dump_Symbol(stream, n+2, type_decl);
   expr->dump(stream, n+2);
}


Expression assign_class::copy_Expression()
{
   return new assign_class(copy_Symbol(name), expr->copy_Expression());
}


void assign_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "assign\n";
   dump_Symbol(stream, n+2, name);
   expr->dump(stream, n+2);
}


Expression static_dispatch_class::copy_Expression()
{
   return new static_dispatch_class(expr->copy_Expression(), copy_Symbol(type_name), copy_Symbol(name), actual->copy_list());
}


void static_dispatch_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "static_dispatch\n";
   expr->dump(stream, n+2);
   dump_Symbol(stream, n+2, type_name);
   dump_Symbol(stream, n+2, name);
   actual->dump(stream, n+2);
}


Expression dispatch_class::copy_Expression()
{
   return new dispatch_class(expr->copy_Expression(), copy_Symbol(name), actual->copy_list());
}


void dispatch_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "dispatch\n";
   expr->dump(stream, n+2);
   dump_Symbol(stream, n+2, name);
   actual->dump(stream, n+2);
}


Expression cond_class::copy_Expression()
{
   return new cond_class(pred->copy_Expression(), then_exp->copy_Expression(), else_exp->copy_Expression());
}


void cond_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "cond\n";
   pred->dump(stream, n+2);
   then_exp->dump(stream, n+2);
   else_exp->dump(stream, n+2);
}


Expression loop_class::copy_Expression()
{
   return new loop_class(pred->copy_Expression(), body->copy_Expression());
}


void loop_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "loop\n";
   pred->dump(stream, n+2);
   body->dump(stream, n+2);
}


Expression typcase_class::copy_Expression()
{
   return new typcase_class(expr->copy_Expression(), cases->copy_list());
}


void typcase_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "typcase\n";
   expr->dump(stream, n+2);
   cases->dump(stream, n+2);
}


Expression block_class::copy_Expression()
{
   return new block_class(body->copy_list());
}


void block_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "block\n";
   body->dump(stream, n+2);
}


Expression let_class::copy_Expression()
{
   return new let_class(copy_Symbol(identifier), copy_Symbol(type_decl), init->copy_Expression(), body->copy_Expression());
}


void let_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "let\n";
   dump_Symbol(stream, n+2, identifier);
   dump_Symbol(stream, n+2, type_decl);
   init->dump(stream, n+2);
   body->dump(stream, n+2);
}


Expression plus_class::copy_Expression()
{
   return new plus_class(e1->copy_Expression(), e2->copy_Expression());
}


void plus_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "plus\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression sub_class::copy_Expression()
{
   return new sub_class(e1->copy_Expression(), e2->copy_Expression());
}


void sub_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "sub\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression mul_class::copy_Expression()
{
   return new mul_class(e1->copy_Expression(), e2->copy_Expression());
}


void mul_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "mul\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression divide_class::copy_Expression()
{
   return new divide_class(e1->copy_Expression(), e2->copy_Expression());
}


void divide_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "divide\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression neg_class::copy_Expression()
{
   return new neg_class(e1->copy_Expression());
}


void neg_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "neg\n";
   e1->dump(stream, n+2);
}


Expression lt_class::copy_Expression()
{
   return new lt_class(e1->copy_Expression(), e2->copy_Expression());
}


void lt_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "lt\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression eq_class::copy_Expression()
{
   return new eq_class(e1->copy_Expression(), e2->copy_Expression());
}


void eq_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "eq\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression leq_class::copy_Expression()
{
   return new leq_class(e1->copy_Expression(), e2->copy_Expression());
}


void leq_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "leq\n";
   e1->dump(stream, n+2);
   e2->dump(stream, n+2);
}


Expression comp_class::copy_Expression()
{
   return new comp_class(e1->copy_Expression());
}


void comp_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "comp\n";
   e1->dump(stream, n+2);
}

Expression int_const_class::copy_Expression()
{
   return new int_const_class(copy_Symbol(token));
}

void int_const_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "int_const\n";
   dump_Symbol(stream, n+2, token);
}


Expression bool_const_class::copy_Expression()
{
   return new bool_const_class(copy_Boolean(val));
}


void bool_const_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "bool_const\n";
   dump_Boolean(stream, n+2, val);
}


Expression string_const_class::copy_Expression()
{
   return new string_const_class(copy_Symbol(token));
}


void string_const_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "string_const\n";
   dump_Symbol(stream, n+2, token);
}


Expression new__class::copy_Expression()
{
   return new new__class(copy_Symbol(type_name));
}


void new__class::dump(ostream& stream, int n)
{
   stream << pad(n) << "new_\n";
   dump_Symbol(stream, n+2, type_name);
}


Expression isvoid_class::copy_Expression()
{
   return new isvoid_class(e1->copy_Expression());
}


void isvoid_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "isvoid\n";
   e1->dump(stream, n+2);
}


Expression no_expr_class::copy_Expression()
{
   return new no_expr_class();
}


void no_expr_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "no_expr\n";
}


Expression object_class::copy_Expression()
{
   return new object_class(copy_Symbol(name));
}


void object_class::dump(ostream& stream, int n)
{
   stream << pad(n) << "object\n";
   dump_Symbol(stream, n+2, name);
}


// interfaces used by Bison
Classes nil_Classes()
{
   return new nil_node<Class_>();
}

Classes single_Classes(Class_ e)
{
   return new single_list_node<Class_>(e);
}

Classes append_Classes(Classes p1, Classes p2)
{
   return new append_node<Class_>(p1, p2);
}

Features nil_Features()
{
   return new nil_node<Feature>();
}

Features single_Features(Feature e)
{
   return new single_list_node<Feature>(e);
}

Features append_Features(Features p1, Features p2)
{
   return new append_node<Feature>(p1, p2);
}

Formals nil_Formals()
{
   return new nil_node<Formal>();
}

Formals single_Formals(Formal e)
{
   return new single_list_node<Formal>(e);
}

Formals append_Formals(Formals p1, Formals p2)
{
   return new append_node<Formal>(p1, p2);
}

Expressions nil_Expressions()
{
   return new nil_node<Expression>();
}

Expressions single_Expressions(Expression e)
{
   return new single_list_node<Expression>(e);
}

Expressions append_Expressions(Expressions p1, Expressions p2)
{
   return new append_node<Expression>(p1, p2);
}

Cases nil_Cases()
{
   return new nil_node<Case>();
}

Cases single_Cases(Case e)
{
   return new single_list_node<Case>(e);
}

Cases append_Cases(Cases p1, Cases p2)
{
   return new append_node<Case>(p1, p2);
}

Program program(Classes classes)
{
  return new program_class(classes);
}

Class_ class_(Symbol name, Symbol parent, Features features, Symbol filename)
{
  return new class__class(name, parent, features, filename);
}

Feature method(Symbol name, Formals formals, Symbol return_type, Expression expr)
{
  return new method_class(name, formals, return_type, expr);
}

Feature attr(Symbol name, Symbol type_decl, Expression init)
{
  return new attr_class(name, type_decl, init);
}

Formal formal(Symbol name, Symbol type_decl)
{
  return new formal_class(name, type_decl);
}

Case branch(Symbol name, Symbol type_decl, Expression expr)
{
  return new branch_class(name, type_decl, expr);
}

Expression assign(Symbol name, Expression expr)
{
  return new assign_class(name, expr);
}

Expression static_dispatch(Expression expr, Symbol type_name, Symbol name, Expressions actual)
{
  return new static_dispatch_class(expr, type_name, name, actual);
}

Expression dispatch(Expression expr, Symbol name, Expressions actual)
{
  return new dispatch_class(expr, name, actual);
}

Expression cond(Expression pred, Expression then_exp, Expression else_exp)
{
  return new cond_class(pred, then_exp, else_exp);
}

Expression loop(Expression pred, Expression body)
{
  return new loop_class(pred, body);
}

Expression typcase(Expression expr, Cases cases)
{
  return new typcase_class(expr, cases);
}

Expression block(Expressions body)
{
  return new block_class(body);
}

Expression let(Symbol identifier, Symbol type_decl, Expression init, Expression body)
{
  return new let_class(identifier, type_decl, init, body);
}

Expression plus(Expression e1, Expression e2)
{
  return new plus_class(e1, e2);
}

Expression sub(Expression e1, Expression e2)
{
  return new sub_class(e1, e2);
}

Expression mul(Expression e1, Expression e2)
{
  return new mul_class(e1, e2);
}

Expression divide(Expression e1, Expression e2)
{
  return new divide_class(e1, e2);
}

Expression neg(Expression e1)
{
  return new neg_class(e1);
}

Expression lt(Expression e1, Expression e2)
{
  return new lt_class(e1, e2);
}

Expression eq(Expression e1, Expression e2)
{
  return new eq_class(e1, e2);
}

Expression leq(Expression e1, Expression e2)
{
  return new leq_class(e1, e2);
}

Expression comp(Expression e1)
{
  return new comp_class(e1);
}

Expression int_const(Symbol token)
{
  return new int_const_class(token);
}

Expression bool_const(Boolean val)
{
  return new bool_const_class(val);
}

Expression string_const(Symbol token)
{
  return new string_const_class(token);
}

Expression new_(Symbol type_name)
{
  return new new__class(type_name);
}

Expression isvoid(Expression e1)
{
  return new isvoid_class(e1);
}

Expression no_expr()
{
  return new no_expr_class();
}

Expression object(Symbol name)
{
  return new object_class(name);
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   expr->code(s, cgen_node, formals_table, sp, num_params);
   int* fp_offset = formals_table.lookup(name->get_string());
   int attribute_location = cgen_node->get_attribute_location(name);
   if (fp_offset != nullptr)
   {
      emit_store(ACC, *fp_offset, FP, s);
   }
   else if (attribute_location != -1)
   {
      //assign to attribute
      emit_store(ACC, 3 + attribute_location, SELF, s);
      
      if (cgen_Memmgr != GC_NOGC)
      {
         emit_addiu(A1, SELF, (3 + attribute_location) * WORD_SIZE, s);
         emit_jal("_GenGC_Assign", sp, 0, s);
      }
   }
   else
   {
      abort(); // Should always be able to find the symbol to assign to
   }
}

// Loads filename into a0 and linenumber into t1 for abort messages
void emit_load_filename_and_line_number(ostream &s, Expression  expr, CgenNodeP cgen_node)
{
   // Load filename into a0
   std::stringstream filename_string_stream;
   std::string filename = cgen_node->get_filename()->get_string();
   StringEntry* string_entry = stringtable.lookup_string(filename.c_str());
   string_entry->code_ref(filename_string_stream);
   emit_load_address(ACC, filename_string_stream.str().c_str(), s);

   // Load line number in T1
   emit_load_imm(T1, expr->get_line_number(), s);
}

void emit_dispatch(ostream &s, Expression expression, Symbol dispatch_type, Symbol method_name, Expressions parameters, CgenNodeP cgen_node, 
SymbolTable<std::string, int> formals_table, int& sp, bool is_dynamic, int num_params)
{
   // Push all of the parameters onto the stack
   for(int i = parameters->first(); parameters->more(i); i = parameters->next(i))
   {
      // Emit code for parameter expression
      parameters->nth(i)->code(s, cgen_node, formals_table, sp, num_params);

      // Copy the parameter to a new location on the heap as specified by cool operational semantics
      // todo: it may be possible to optimize this out in certain situations
      //emit_object_copy(s);

      emit_store(ACC, 0, SP, s);

      // bump the stack pointer
      emit_stack_size_push(1, sp, s); 
   }

   // Emit the code for the self object we are dispatching to
   expression->code(s, cgen_node, formals_table, sp, num_params);

   int continue_dispatch = label_index++;

   // If ACC == ZERO load line number in t1 and filename in a0 and call _dispatch_abort
   emit_bne(ACC, ZERO, continue_dispatch, s);

   emit_load_filename_and_line_number(s, expression, cgen_node);
   
   // Jump to dispatch abort
   emit_jump("_dispatch_abort", s);

   // If the expression was not void continue with the dispatch
   emit_label_def(continue_dispatch, s);
   // Get the index into the dispatch table for this method
   Symbol expr_type = dispatch_type == SELF_TYPE ? cgen_node->name : dispatch_type;
   CgenNodeP dispatch_cgen_node = cgen_node->get_symbol_table()->lookup(expr_type);
   int method_index = dispatch_cgen_node->get_method_location(method_name);
   assert(method_index != -1);

   if (is_dynamic == false)
   {
      // Load the proto obect for the dispatch type into T0 
      emit_partial_load_address(T0, s);
      emit_protobj_ref(expr_type, s);
      s << endl;
   }
   else
   {
      // Moving the dispatch type into TO 
      emit_move(T0, ACC, s);
   }

   // Load the address of the dispatch table into T0
   emit_load(T0, 2, T0, s);

   // Add the offset for the method location in the dispatch table
   emit_addiu(T0, T0, method_index * WORD_SIZE,  s);

   // Method address is now loaded into T0
   emit_load(T0, 0, T0, s);

   // Jal to the method definition
   emit_jalr(T0, sp, parameters->len(), s);
}

void static_dispatch_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   emit_dispatch(s, expr, type_name, name, actual, cgen_node, formals_table, sp, false, num_params);
}

void dispatch_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   emit_dispatch(s, expr, expr->type, name, actual, cgen_node, formals_table, sp, true, num_params);
}

void cond_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   int else_label = label_index++;
   int exit_label = label_index++;

   // first emit the code to evaluate the predicate
   pred->code(s, cgen_node, formals_table, sp, num_params);

   // after the predicate is evaluated we need to test to see if it is true or false
   // to do that we:
   // First load boolconst1 into T1
   emit_load_bool(T1, BoolConst(1), s);

   // Then load the result of the pred expression into T2
   emit_add(T2, ACC, ZERO, s);

   // Value returned in ACC if true
   emit_load_imm(ACC, 1, s);

   // Value return in ACC if false
   emit_load_imm(A1, 0, s);

   // Jump and link to the compare method to test the predicate result
   emit_jal("equality_test", sp, 0, s);

   // Jump to else if false
   emit_beq(ACC, ZERO, else_label, s);
   // evaluate the then block if true
   then_exp->code(s, cgen_node, formals_table, sp, num_params);
   emit_jump(exit_label, s);

   emit_label_def(else_label, s);
   // evaluate else block if false
   else_exp->code(s, cgen_node, formals_table, sp, num_params);

   emit_label_def(exit_label, s);
}

void loop_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   int exit_label = label_index++;
   int loop_label = label_index++;
   int evaluate_pred_label = label_index++;

   // first emit the code to evaluate the predicate
   emit_label_def(evaluate_pred_label, s);
   pred->code(s, cgen_node, formals_table, sp, num_params);

   // after the predicate is evaluated we need to test to see if it is true or false
   // to do that we:
   // First load boolconst1 into T1
   emit_load_bool(T1, BoolConst(1), s);

   // Then load the result of the pred expression into T2
   emit_add(T2, ACC, ZERO, s);

   // Value returned in ACC if true
   emit_load_imm(ACC, 1, s);

   // Value return in ACC if false
   emit_load_imm(A1, 0, s);

   // Jump and link to the compare method to test the predicate result
   emit_jal("equality_test", sp, 0, s);

   emit_beq(ACC, ZERO, exit_label, s);

   emit_label_def(loop_label, s);
   body->code(s, cgen_node, formals_table, sp, num_params);
   emit_jump(evaluate_pred_label, s);

   emit_label_def(exit_label, s);
   // loop always returns void
   emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   // First emit code to generate the predicate
   expr->code(s, cgen_node, formals_table, sp, num_params);

   int continue_case = label_index++;

   // If ACC == ZERO load line number in t1 and filename in a0 and call _case_abort2
   emit_bne(ACC, ZERO, continue_case, s);

   emit_load_filename_and_line_number(s, expr, cgen_node);
   
   emit_jump("_case_abort2", s);

   // If the predicate was not void continue with the case state
   emit_label_def(continue_case, s);

   // First get the class tag for the case expression class
   emit_load(T1, 0, ACC, s);

   // Load word size into T2
   emit_load_imm(T2, WORD_SIZE, s);
   // Multiply T1 by the number of bytes which is stored in T2 to get the offset from the class_tagTab label
   emit_mul(T1, T1, T2, s);

   //The correct class tag table offset is now stored in T1, now we need to use the class tag table to access the inheritance table for this class
   emit_load_address(T2, CLASSTAGTAB, s);
   //T1 now stores the location in memory of a word that stores the address to the inheritance table
   emit_add(T1, T2, T1, s);

   //T0 now stores the address of the inheritance table
   emit_load(T0, 0, T1, s);

   std::vector<branch_class*> case_branches;
   // Sorting the branch objects based on their inheritance distance to object
   for(int i = cases->first(); cases->more(i); i = cases->next(i))
   {
      case_branches.push_back(static_cast<branch_class*>(cases->nth(i)));
   }

   // Now for each of these branches I need to sort them based on their inheritance 
   std::sort(case_branches.begin(), case_branches.end(), [cgen_node](branch_class* first, branch_class* second) {
      CgenNodeP first_branch_type = cgen_node->get_symbol_table()->lookup(first->type_decl);
      CgenNodeP second_branch_type = cgen_node->get_symbol_table()->lookup(second->type_decl);

      return first_branch_type->get_inheritance_depth() > second_branch_type->get_inheritance_depth();
   });

   int case_finished_label = label_index++;

   // Now start evaluating all of the branches from the deepest inheritance to the least deep (progressing towards object)
   for(auto it = case_branches.cbegin(); it != case_branches.cend(); ++it)
   {
      int case_match_found_label = label_index++;
      int case_branch_loop_label = label_index++;
      int case_branch_exit_label = label_index++;

      branch_class* caseBranch = *it;

      // Load the address of the inheritance table into T1
      emit_addiu(T1, T0, 0, s);

      // Load the proto obj address into T2
      emit_partial_load_address(T2, s);
      emit_protobj_ref(caseBranch->type_decl, s);
      s << endl;

      // Load the class tag for this object into T2
      emit_load(T2, 0, T2, s);

      // Now we have the class tag of the branch type in T2 and the address of the inheritance table in T1

      // Now we need to interate up the inheritance table until we get a match

      // Load the class tag of the inheritance hierarchy pointed to by T1 into T3
      emit_label_def(case_branch_loop_label, s);
      emit_load(T3, 0, T1, s);

      // If T3 is equal to 1 it means there was no match for this branch, exit the loop.
      emit_addiu(T4, ZERO, 1, s);
      emit_beq(T3, T4, case_branch_exit_label, s);

      // If the loaded tag is equal to the tag stored in T2 we have a match
      emit_beq(T3, T2, case_match_found_label, s);

      // if not equal then increment and loop
      emit_addiu(T1, T1, 4, s);
      emit_jump(case_branch_loop_label, s);
      
      emit_label_def(case_match_found_label, s);

      // add the case statement definition to the scope

      // Push the result of the copy onto the stack
      emit_store(ACC, 0, SP, s);

      // bump the stack pointer
      emit_stack_size_push(1, sp, s);

      // code gen for the case statement branch
      formals_table.enterscope();

      // Keep track of the case binding
      int* fp_offset = new int;
      // todo: This is really gross, I need to figure out what the relationship is between sp, num_params, and the fp offset. I think I can simplify this 
      *fp_offset = 1 + sp - num_params;
      formals_table.addid(caseBranch->name->get_string(), fp_offset);
      
      caseBranch->expr->code(s, cgen_node, formals_table, sp, num_params);

      formals_table.exitscope();

      // Restore the stack pointer
      emit_stack_size_pop(1, sp, s);

      // Jump out of the case expression
      emit_jump(case_finished_label, s);

      emit_label_def(case_branch_exit_label, s);
   }
   
   emit_label_def(case_finished_label, s);

   // If T3 is equal to 1 upon testing all of the branches it means there was no match
   emit_addiu(T4, ZERO, 1, s);
   emit_beq(T3, T4, "_case_abort", s);
}

void block_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   for(int i = body->first(); body->more(i); i = body->next(i))
   {
      body->nth(i)->code(s, cgen_node, formals_table, sp, num_params);
   }
}

void let_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params) {
   bool isBasic = (type_decl == Int || type_decl == Str || type_decl == Bool);

   // If the type is an Int or String and there is no expression, init with default proto-obj, otherwise emit code for the initialization expression
   if (dynamic_cast<no_expr_class*>(init) != nullptr && isBasic)
   {
      // If we don't have an expression init the attribute to the value of the appropiate proto obj
      // Load the proto object address into ACC
      // Note that for non-basic types we just let them initialize to void
      emit_partial_load_address(ACC, s);
      emit_protobj_ref(type_decl, s);
      s << endl;

      emit_object_copy(s);
   }
   else
   {
      // Emit code for let variable initialization
      // Note: this will copy zero into ACC for no_expr_class
      init->code(s, cgen_node, formals_table, sp, num_params);
   }

   // Push the result of the copy onto the stack
   emit_store(ACC, 0, SP, s);

   // bump the stack pointer
   emit_stack_size_push(1, sp, s);

   formals_table.enterscope();

   int* fp_offset = new int;
   *fp_offset = 1 + sp - num_params;
   formals_table.addid(identifier->get_string(), fp_offset);

   // Then evaluate the body of the let
   body->code(s, cgen_node, formals_table, sp, num_params);

   formals_table.exitscope();

   // Restore the stack pointer
   emit_stack_size_pop(1, sp, s);
}

void emit_binary_op_prefix(Expression lhs, Expression rhs, ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int> formals_table, int& sp, int num_params)
{
  // This function assumes that Int objects will be present in register ACC after evaluating both lhs and rhs expressions

  lhs->code(s, cgen_node, formals_table, sp, num_params);

  // push lhs result to stack
  emit_store(ACC, 0, SP, s);
  // bump the stack pointer
  emit_stack_size_push(1, sp, s);

  rhs->code(s, cgen_node, formals_table, sp, num_params);

  // load the memory address of the object into T2
  emit_load(T2, 1, SP, s);

  // load the int result from the lhs into T1
  emit_load(T1, 3, T2, s);

  // Load the int result from the rhs into T2
  emit_load(T2, 3, ACC, s);

  //T1 now stores the lhs int operand and T2 now stores the rhs int operand-
}

void emit_object_allocation(Symbol return_type, ostream &s , int& sp)
{
  /* Create a new return object on the heap, populate its data field with the value in ACC, and populate ACC with a pointer to that object
      Note: Expects the raw result of the operand to be in register T2 */

  // Load the proto object address into ACC
  emit_partial_load_address(ACC, s);
  emit_protobj_ref(return_type, s);
  s << endl;

  // Copy the object onto the heap
  emit_object_copy(s);

  // Store the result in T2 into the object stored at ACC
  emit_store(T2, 3, ACC, s);
}

// todo: Why do the results of expressions get allocated on the heap? How does that make any sense?
// Why couldn't we allocate the result of expressions on the stack?

void emit_binary_op_suffix(Symbol return_type, ostream &s, int& sp)
{
  // For bools we don't need to allocate a new object
  if (return_type == Int) emit_object_allocation(return_type, s, sp);

  // return the stack pointer to its previous value
  emit_stack_size_pop(1, sp, s);
}

void plus_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);

  // add T1 + T2
  emit_add(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s, sp);
}

void sub_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);

  // sub T1 - T2
  emit_sub(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s, sp);
}

void mul_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);

  // mul T1 * T2
  emit_mul(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s, sp);
}

void divide_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);

  // mul T1 / T2
  emit_div(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s, sp);
}

void neg_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
    e1->code(s, cgen_node, formals_table, sp, num_params);

    // Load the int value into ACC
    emit_load(ACC, 3, ACC, s);

    // Load -1 into T1
    emit_load_imm(T1, -1, s);

    // Negate the int value by multiplying by -1
    emit_mul(T2, ACC, T1, s);

    // Create a new Int object on the heap and store the value of T2 in it
    emit_object_allocation(Int, s, sp);
}

void lt_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
   emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);
   int true_branch_label = label_index++;
   int false_branch_label = label_index++;

   emit_blt(T1, T2, true_branch_label, s);
   emit_load_bool(ACC, BoolConst(0), s);
   emit_jump(false_branch_label, s);
   emit_label_def(true_branch_label, s);
   emit_load_bool(ACC, BoolConst(1), s);
   emit_label_def(false_branch_label, s);

   emit_binary_op_suffix(Bool, s, sp);
}

void leq_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
   emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, sp, num_params);
   int true_branch_label = label_index++;
   int false_branch_label = label_index++;

   emit_bleq(T1, T2, true_branch_label, s);
   emit_load_bool(ACC, BoolConst(0), s);
   emit_jump(false_branch_label, s);
   emit_label_def(true_branch_label, s);
   emit_load_bool(ACC, BoolConst(1), s);
   emit_label_def(false_branch_label, s);

   emit_binary_op_suffix(Bool, s, sp);
}

void eq_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  e1->code(s, cgen_node, formals_table, sp, num_params);
  // Push result of LHS onto the stack
  emit_store(ACC, 0, SP, s);
  emit_stack_size_push(1, sp, s);

  e2->code(s, cgen_node, formals_table, sp, num_params);
  // move RHS into T2
  emit_move(T2, ACC, s);
  // load LHS into T1
  emit_load(T1, 1, SP, s);

  // Value returned in ACC if true
  emit_load_bool(ACC, BoolConst(1), s);

  // If pointers are equal we don't need to do the equality test jal 
  int pointers_are_equal = label_index++; 
  emit_beq(T2, T1, pointers_are_equal, s);

  // Pointers not equal so continue with equality test
  // Value return in ACC if false
  emit_load_bool(A1, BoolConst(0), s);

  // Jump and link to the compare method
  emit_jal("equality_test", sp, 0, s);

  emit_label_def(pointers_are_equal, s);
  // Restore the stack pointer
  emit_stack_size_pop(1, sp, s);
}

// Note: this is actually the not operator. No idea why it is called comp
void comp_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
   int false_label = label_index++;
   int true_label = label_index++;
   // this produces a bool value in acc
   e1->code(s, cgen_node, formals_table, sp, num_params);

   // Load the bool value into ACC
   emit_load(ACC, 3, ACC, s);

   // Inverts the bool object and stores result in T2. In other words if ACC < 1 sets ACC to 1 otherwise sets it to 0
   emit_slti(T2, ACC, 1, s);

   int zero_label = label_index++;
   int one_label = label_index++;

   // If T2 is Zero then return BoolConst(0) object, otherwise return BoolConst(1) object
   emit_beq(T2, ZERO, zero_label, s);
   emit_load_bool(ACC, BoolConst(1), s);
   emit_jump(one_label, s);
   emit_label_def(zero_label, s);
   emit_load_bool(ACC, BoolConst(0), s);
   emit_label_def(one_label, s);
}

void int_const_class::code(ostream& s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int&, int)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int&, int)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenNodeP cgen_node,  SymbolTable<std::string, int>& formals_table, int&, int)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
   if (type_name == SELF_TYPE)
   {
      // First load the class tag for the SELF object into T1
      emit_load(T1, 0, SELF, s);

      // Load word size * 2 into T2 since the proto-object labels are stored every 2 words / 8 bytes
      emit_load_imm(T2, WORD_SIZE * 2, s);
      // Multiply T1 by the number of bytes which is stored in T2 to get the offset from the class_objTab label
      emit_mul(T1, T1, T2, s);

      //The correct class tag table offset is now stored in T1, now we need to use the class_objTab to access the proto-obj for this class
      emit_load_address(T2, CLASSOBJTAB, s);
      //T1 now stores the location in memory of a word that stores the address to the proto-object
      emit_add(T1, T2, T1, s);

      //ACC now stores the address of the correct self proto-object
      emit_load(ACC, 0, T1, s);

      // Add one word to T1 to get the address of the init method
      emit_addiu(T1, T1, WORD_SIZE, s);

      // Load the address of the init method pointed to by T1 into T1
      emit_load(T1, 0, T1, s);
      
      // Push the value onto the stack for later retrieval
      emit_store(T1, 0, SP, s);
      emit_stack_size_push(1, sp, s);
   }
   else
   {
      // Load the correct proto-object into ACC
      emit_partial_load_address(ACC, s);
      emit_protobj_ref(type_name, s);
      s << endl;
   }

   // Copy the proto-object to the heap
   emit_object_copy(s);

   if (type_name == SELF_TYPE)
   {
      // load the address of the init method from the stack
      emit_load(T1, 1, SP, s);
      emit_stack_size_pop(1, sp, s);

      // jump to init method
      emit_jalr(T1, sp, 0, s);
   }
   else
   {
      // Invoke initialization method on the object that we just allocated
      std::stringstream init_method_ref;
      emit_init_ref(type_name, init_method_ref);
      emit_jal(init_method_ref.str().c_str(), sp, 0, s);
   }
}

void isvoid_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  e1->code(s, cgen_node, formals_table, sp, num_params);

  int zero_label = label_index++;
  int one_label = label_index++;

  emit_beq(ACC, ZERO, zero_label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_jump(one_label, s);
  emit_label_def(zero_label, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_label_def(one_label, s);
}

void no_expr_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
  // Load void into ACC for no_expr
  emit_load_imm(ACC, 0, s);
}

void object_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& sp, int num_params)
{
   if (name == self) 
   {
      //todo: I am a bit worried here because I don't know if S0 will always store a pointer to self or not
      //todo: Convince yourself that S0 will always contain a pointer to self
      emit_move(ACC, SELF, s);
   }
   else if (formals_table.lookup(name->get_string()) != nullptr)
   {
      int* fp_offset = formals_table.lookup(name->get_string());
      emit_load(ACC, *fp_offset, FP, s);
   } 
   else
   {
      // else look for a matching attribute in the class
      int attribute_location = cgen_node->get_attribute_location(name);
      assert(attribute_location != -1);
      emit_load(ACC, 3 + attribute_location, SELF, s);
   }
}

// TAC Generation for Expressions

std::string new_temp(int& temp_counter) {
    return "t" + std::to_string(temp_counter++);
}

void assign_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string rhs = new_temp(temp_counter);
    expr->code_ir(ir, rhs, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, rhs)));
   // no idea what the AI added this  
   //  if (!dst.empty())
   //      ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, rhs)));
}

void static_dispatch_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    for (int i = 0; actual->more(i); i = actual->next(i)) {
        std::string arg_temp = new_temp(temp_counter);
        actual->nth(i)->code_ir(ir, arg_temp, temp_counter);
        ir.push_back(IRInstruction(IROpcode::IR_PARAM, IROperand(IROperand::Kind::VAR, arg_temp)));
    }
    std::string obj_temp = new_temp(temp_counter);
    expr->code_ir(ir, obj_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_PARAM, IROperand(IROperand::Kind::VAR, obj_temp)));
    ir.push_back(IRInstruction(IROpcode::IR_CALL, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, name->get_string())));
}

void dispatch_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    for (int i = 0; actual->more(i); i = actual->next(i)) {
        std::string arg_temp = new_temp(temp_counter);
        actual->nth(i)->code_ir(ir, arg_temp, temp_counter);
        ir.push_back(IRInstruction(IROpcode::IR_PARAM, IROperand(IROperand::Kind::VAR, arg_temp)));
    }
    std::string obj_temp = new_temp(temp_counter);
    expr->code_ir(ir, obj_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_PARAM, IROperand(IROperand::Kind::VAR, obj_temp)));
    ir.push_back(IRInstruction(IROpcode::IR_CALL, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, name->get_string())));
}

void cond_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string pred_temp = new_temp(temp_counter);
    pred->code_ir(ir, pred_temp, temp_counter);
    std::string then_temp = new_temp(temp_counter);
    std::string else_temp = new_temp(temp_counter);
    std::string then_label = "then_" + std::to_string(temp_counter++);
    std::string else_label = "else_" + std::to_string(temp_counter++);
    std::string end_label = "endif_" + std::to_string(temp_counter++);
    ir.push_back(IRInstruction(IROpcode::IR_IF_GOTO, IROperand(IROperand::Kind::VAR, pred_temp)));
    ir.back().label = then_label;
    ir.push_back(IRInstruction(IROpcode::IR_GOTO, else_label));
    ir.push_back(IRInstruction(IROpcode::IR_LABEL, then_label));
    then_exp->code_ir(ir, then_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, then_temp)));
    ir.push_back(IRInstruction(IROpcode::IR_GOTO, end_label));
    ir.push_back(IRInstruction(IROpcode::IR_LABEL, else_label));
    else_exp->code_ir(ir, else_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, else_temp)));
    ir.push_back(IRInstruction(IROpcode::IR_LABEL, end_label));
}

void loop_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string pred_temp = new_temp(temp_counter);
    std::string body_temp = new_temp(temp_counter);
    std::string loop_label = "loop_" + std::to_string(temp_counter++);
    std::string end_label = "endloop_" + std::to_string(temp_counter++);
    ir.push_back(IRInstruction(IROpcode::IR_LABEL, loop_label));
    pred->code_ir(ir, pred_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_IF_GOTO, IROperand(IROperand::Kind::VAR, pred_temp)));
    ir.back().label = end_label;
    body->code_ir(ir, body_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_GOTO, loop_label));
    ir.push_back(IRInstruction(IROpcode::IR_LABEL, end_label));
    if (!dst.empty())
        ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, body_temp)));
}

void typcase_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string expr_temp = new_temp(temp_counter);
    expr->code_ir(ir, expr_temp, temp_counter);
    if (cases->len() > 0) {
        branch_class* first_branch = static_cast<branch_class*>(cases->nth(0));
        std::string branch_temp = new_temp(temp_counter);
        first_branch->expr->code_ir(ir, branch_temp, temp_counter);
        ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, branch_temp)));
    }
}

void block_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string last_temp;
    for (int i = 0; body->more(i); i = body->next(i)) {
        last_temp = new_temp(temp_counter);
        body->nth(i)->code_ir(ir, last_temp, temp_counter);
    }
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, last_temp)));
}

void let_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string init_temp = new_temp(temp_counter);
    init->code_ir(ir, init_temp, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, identifier->get_string()), IROperand(IROperand::Kind::VAR, init_temp)));
    body->code_ir(ir, dst, temp_counter);
}

void plus_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
  std::string left = new_temp(temp_counter);
  std::string right = new_temp(temp_counter);
  e1->code_ir(ir, left, temp_counter);
  e2->code_ir(ir, right, temp_counter);
  ir.push_back(IRInstruction(IROpcode::IR_ADD, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void sub_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_SUB, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void mul_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_MUL, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void divide_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_DIV, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void neg_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string src = new_temp(temp_counter);
    e1->code_ir(ir, src, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_NEG, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, src)));
}

void lt_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_LT, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void leq_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_LEQ, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void eq_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string left = new_temp(temp_counter);
    std::string right = new_temp(temp_counter);
    e1->code_ir(ir, left, temp_counter);
    e2->code_ir(ir, right, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_EQ, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, left), IROperand(IROperand::Kind::VAR, right)));
}

void comp_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string src = new_temp(temp_counter);
    e1->code_ir(ir, src, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_COMP, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, src)));
}

void int_const_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_CONST, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::CONST_INT, token->get_string())));
}

void bool_const_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_CONST, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::CONST_BOOL, val ? "true" : "false")));
}

void string_const_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_CONST, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::CONST_STR, token->get_string())));
}

void new__class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_CALL, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, type_name->get_string())));
}

void isvoid_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    std::string src = new_temp(temp_counter);
    e1->code_ir(ir, src, temp_counter);
    ir.push_back(IRInstruction(IROpcode::IR_COMP, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, src)));
}

void no_expr_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_CONST, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::CONST_INT, "0")));
}

void object_class::code_ir(std::vector<IRInstruction>& ir, std::string dst, int& temp_counter) {
    ir.push_back(IRInstruction(IROpcode::IR_ASSIGN, IROperand(IROperand::Kind::VAR, dst), IROperand(IROperand::Kind::VAR, name->get_string())));
}
