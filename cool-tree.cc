//////////////////////////////////////////////////////////
//
// file: cool-tree.cc
//
// This file defines the functions of each class
//
//////////////////////////////////////////////////////////

#include <sstream>
#include <algorithm>
#include <set>
#include "tree.h"
#include "cool-tree.h"
#include "cgen.h"
#include "emit.h"
#include "cgen_gc.h"

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

void assign_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   expr->emit_ir(tac_statements,cgen_node, formals_table, local_index);
   int* fp_offset = formals_table.lookup(name->get_string());
   int attribute_location = cgen_node->get_attribute_location(name);
   if (fp_offset != nullptr)
   {
      tac_statements.push_back(std::make_unique<IRStore>(IROperand(FP), IROperand(ACC), IROperand(*fp_offset)));
   }
   else if (attribute_location != -1)
   {
      //assign to attribute
      tac_statements.push_back(std::make_unique<IRStore>(IROperand(SELF), IROperand(ACC), IROperand(DEFAULT_OBJFIELDS + attribute_location)));
      
      if (cgen_Memmgr != GC_NOGC)
      {
         tac_statements.push_back(std::make_unique<IRAdd>(IROperand(A1), IROperand(SELF), IROperand((DEFAULT_OBJFIELDS + attribute_location) * WORD_SIZE)));
         tac_statements.push_back(std::make_unique<IRLabelJumpAndLink>("_GenGC_Assign"));
      }
   }
   else
   {
      abort(); // Should always be able to find the symbol to assign to
   }
}

void assign_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   expr->code(s, cgen_node, formals_table, local_index);
   int* fp_offset = formals_table.lookup(name->get_string());
   int attribute_location = cgen_node->get_attribute_location(name);
   if (fp_offset != nullptr)
   {
      emit_store(ACC, *fp_offset, FP, s);
   }
   else if (attribute_location != -1)
   {
      //assign to attribute
      emit_store(ACC, DEFAULT_OBJFIELDS + attribute_location, SELF, s);
      
      if (cgen_Memmgr != GC_NOGC)
      {
         emit_addiu(A1, SELF, (DEFAULT_OBJFIELDS + attribute_location) * WORD_SIZE, s);
         emit_jal("_GenGC_Assign", s);
      }
   }
   else
   {
      abort(); // Should always be able to find the symbol to assign to
   }
}

// Loads filename into a0 and linenumber into t1 for abort messages
void emit_load_filename_and_line_number_ir(IRStatements& tac_statements, Expression expr, CgenNodeP cgen_node)
{
   // Load filename into a0
   std::string filename = cgen_node->get_filename()->get_string();
   StringEntry* string_entry = stringtable.lookup_string(filename.c_str());
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_str_const_ref(string_entry)), IROperand(0)));

   // Load line number in T1
   tac_statements.push_back(std::make_unique<IRAssign>(IROperand(T1), expr->get_line_number()));
}

// Loads filename into a0 and linenumber into t1 for abort messages
void emit_load_filename_and_line_number(ostream &s, Expression  expr, CgenNodeP cgen_node)
{
   // Load filename into a0
   std::string filename = cgen_node->get_filename()->get_string();
   StringEntry* string_entry = stringtable.lookup_string(filename.c_str());
   emit_load_address(ACC, get_str_const_ref(string_entry).c_str(), s);

   // Load line number in T1
   emit_load_imm(T1, expr->get_line_number(), s);
}

void emit_dispatch_ir(IRStatements& tac_statements, Expression expression, Symbol dispatch_type, Symbol method_name, Expressions parameters, CgenNodeP cgen_node, 
SymbolTable<std::string, int> formals_table, int& local_index, bool is_dynamic)
{
   // Push all of the parameters onto the stack
   for(int i = parameters->first(); parameters->more(i); i = parameters->next(i))
   {
      // Emit code for parameter expression
      parameters->nth(i)->emit_ir(tac_statements, cgen_node, formals_table, local_index);

      // Copy the parameter to a new location on the heap as specified by cool operational semantics
      // todo: Doesn't seem to be needed because I fail a test with this not commented out
      // emit_object_copy(s);

      tac_statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(ACC), IROperand(0)));
      // emit_store(ACC, 0, SP, s);
      // tac_statements.push_back(std::make_unique<IRPlus>(IROperand(A1), IROperand(SELF), IROperand((DEFAULT_OBJFIELDS + attribute_location) * WORD_SIZE)));
      // bump the stack pointer
      append_ir_stack_size_push(tac_statements, 1);
   }

   // Emit the code for the self object we are dispatching to
   expression->emit_ir(tac_statements, cgen_node, formals_table, local_index);

   int continue_dispatch = label_index++;

   // If ACC == ZERO load line number in t1 and filename in a0 and call _dispatch_abort
   IRRelOp rel_op = IRRelOp(IROperand(ACC), IROperand(ZERO), IRRelOp::Kind::IR_NEQ);
   tac_statements.push_back(std::make_unique<IRIfJump>(rel_op, get_label_ref(continue_dispatch)));

   emit_load_filename_and_line_number_ir(tac_statements, expression, cgen_node);
   
   // Jump to dispatch abort
   tac_statements.push_back(std::make_unique<IRLabelJump>("_dispatch_abort"));

   // If the expression was not void continue with the dispatch
   tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(continue_dispatch)));

   // Get the index into the dispatch table for this method
   Symbol expr_type = dispatch_type == SELF_TYPE ? cgen_node->name : dispatch_type;
   CgenNodeP dispatch_cgen_node = cgen_node->get_symbol_table()->lookup(expr_type);
   int method_index = dispatch_cgen_node->get_method_location(method_name);
   assert(method_index != -1);

   if (is_dynamic == false)
   {
      // Load the proto object for the dispatch type into T0
      tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T0), IRLabelOperand(get_protobj_ref(expr_type)), 0));
   }
   else
   {
      // Moving the dispatch type into TO 
      tac_statements.push_back(std::make_unique<IRMove>(IROperand(T0), IROperand(ACC)));
   }

   // Load the address of the dispatch table into T0
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T0), IROperand(T0), IROperand(2)));

   // Add the offset for the method location in the dispatch table
   tac_statements.push_back(std::make_unique<IRAdd>(IROperand(T0), IROperand(T0), method_index * WORD_SIZE));

   // Method address is now loaded into T0
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T0), IROperand(T0), IROperand(0)));

   // Jal to the method definition
   tac_statements.push_back(std::make_unique<IRRegJumpAndLink>(T0));
}

void emit_dispatch(ostream &s, Expression expression, Symbol dispatch_type, Symbol method_name, Expressions parameters, CgenNodeP cgen_node, 
SymbolTable<std::string, int> formals_table, int& local_index, bool is_dynamic)
{
   // Push all of the parameters onto the stack
   for(int i = parameters->first(); parameters->more(i); i = parameters->next(i))
   {
      // Emit code for parameter expression
      parameters->nth(i)->code(s, cgen_node, formals_table, local_index);

      // Copy the parameter to a new location on the heap as specified by cool operational semantics
      // todo: Doesn't seem to be needed because I fail a test with this not commented out
      // emit_object_copy(s);

      emit_store(ACC, 0, SP, s);
      
      // bump the stack pointer
      emit_stack_size_push(1, s);
   }

   // Emit the code for the self object we are dispatching to
   expression->code(s, cgen_node, formals_table, local_index);

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
   emit_jalr(T0, s);
}

void static_dispatch_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   emit_dispatch_ir(tac_statements, expr, type_name, name, actual, cgen_node, formals_table, local_index, false);
}

void static_dispatch_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   emit_dispatch(s, expr, type_name, name, actual, cgen_node, formals_table, local_index, false);
}

void dispatch_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   emit_dispatch_ir(tac_statements, expr, expr->type, name, actual, cgen_node, formals_table, local_index, true);
}

void dispatch_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   emit_dispatch(s, expr, expr->type, name, actual, cgen_node, formals_table, local_index, true);
}

void cond_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    int else_label = label_index++;
    int exit_label = label_index++;

    // Evaluate predicate
    pred->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Load BoolConst(1) into T1
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T1), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));
    // Move ACC (predicate result) into T2
    tac_statements.push_back(std::make_unique<IRMove>(IROperand(T2), IROperand(ACC)));
    // Set ACC to 1 (true)
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(ACC), 1));
    // Set A1 to 0 (false)
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(A1), 0));
    // Simulate call to equality_test (could be a function call or inlined logic)
    tac_statements.push_back(std::make_unique<IRLabelJumpAndLink>("equality_test"));
    // If ACC == 0, branch to else_label
    IRRelOp cond(IROperand(ACC), IROperand(0), IRRelOp::Kind::IR_EQ);
    tac_statements.push_back(std::make_unique<IRIfJump>(cond, get_label_ref(else_label)));

    // Then branch
    then_exp->emit_ir(tac_statements, cgen_node, formals_table, local_index);
    tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(exit_label)));

    // Else branch
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(else_label)));
    else_exp->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Exit label
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(exit_label)));
}

void cond_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   int else_label = label_index++;
   int exit_label = label_index++;

   // first emit the code to evaluate the predicate
   pred->code(s, cgen_node, formals_table, local_index);

   // after the predicate is evaluated we need to test to see if it is true or false
   // to do that we:
   // First load boolconst1 into T1
   emit_load_bool(T1, BoolConst(1), s);

   // Then load the result of the pred expression into T2
   emit_move(T2, ACC, s);

   // Value returned in ACC if true
   emit_load_imm(ACC, 1, s);

   // Value return in ACC if false
   emit_load_imm(A1, 0, s);

   // Jump and link to the compare method to test the predicate result
   emit_jal("equality_test", s);

   // Jump to else if false
   emit_beq(ACC, ZERO, else_label, s);
   // evaluate the then block if true
   then_exp->code(s, cgen_node, formals_table, local_index);
   emit_jump(exit_label, s);

   emit_label_def(else_label, s);
   // evaluate else block if false
   else_exp->code(s, cgen_node, formals_table, local_index);

   emit_label_def(exit_label, s);
}

void loop_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    int exit_label = label_index++;
    int loop_label = label_index++;
    int evaluate_pred_label = label_index++;

    // Label for evaluating the predicate
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(evaluate_pred_label)));

    // Emit IR for predicate
    pred->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Load BoolConst(1) into T1
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T1), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));
    // Move ACC (predicate result) into T2
    tac_statements.push_back(std::make_unique<IRMove>(IROperand(T2), IROperand(ACC)));
    // Set ACC to 1 (true)
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(ACC), 1));
    // Set A1 to 0 (false)
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(A1), 0));
    // Simulate call to equality_test
    tac_statements.push_back(std::make_unique<IRLabelJumpAndLink>("equality_test"));
    // If ACC == 0, branch to exit_label
    IRRelOp cond(IROperand(ACC), IROperand(0), IRRelOp::Kind::IR_EQ);
    tac_statements.push_back(std::make_unique<IRIfJump>(cond, get_label_ref(exit_label)));

    // Label for loop body
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(loop_label)));
    // Emit IR for loop body
    body->emit_ir(tac_statements, cgen_node, formals_table, local_index);
    // Jump back to predicate evaluation
    tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(evaluate_pred_label)));

    // Exit label
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(exit_label)));

    // Loop always returns void (ACC = 0)
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(ACC), 0));
}

void loop_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   int exit_label = label_index++;
   int loop_label = label_index++;
   int evaluate_pred_label = label_index++;

   // first emit the code to evaluate the predicate
   emit_label_def(evaluate_pred_label, s);
   pred->code(s, cgen_node, formals_table, local_index);

   // after the predicate is evaluated we need to test to see if it is true or false
   // to do that we:
   // First load boolconst1 into T1
   emit_load_bool(T1, BoolConst(1), s);

   // Then load the result of the pred expression into T2
   emit_move(T2, ACC, s);

   // Value returned in ACC if true
   emit_load_imm(ACC, 1, s);

   // Value return in ACC if false
   emit_load_imm(A1, 0, s);

   // Jump and link to the compare method to test the predicate result
   emit_jal("equality_test", s);

   emit_beq(ACC, ZERO, exit_label, s);

   emit_label_def(loop_label, s);
   body->code(s, cgen_node, formals_table, local_index);
   emit_jump(evaluate_pred_label, s);

   emit_label_def(exit_label, s);
   // loop always returns void
   emit_load_imm(ACC, 0, s);
}

void typcase_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    // Evaluate the case expression
    expr->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    int continue_case = label_index++;
    // If ACC == ZERO, abort
    IRRelOp not_void_cond(IROperand(ACC), IROperand(ZERO), IRRelOp::Kind::IR_NEQ);
    tac_statements.push_back(std::make_unique<IRIfJump>(not_void_cond, get_label_ref(continue_case)));

    emit_load_filename_and_line_number_ir(tac_statements, expr, cgen_node);
    tac_statements.push_back(std::make_unique<IRLabelJump>("_case_abort2"));

    // Continue with case statement
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(continue_case)));

    // Get class tag for the case expression
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T1), IROperand(ACC), IROperand(0)));
    // Load word size into T2
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(T2), WORD_SIZE));
    // Multiply T1 by WORD_SIZE
    tac_statements.push_back(std::make_unique<IRMul>(IROperand(T1), IROperand(T1), IROperand(T2)));
    // Load address of class_tag table into T2
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T2), IRLabelOperand(CLASSTAGTAB), IROperand(0)));
    // Add offset to T1
    tac_statements.push_back(std::make_unique<IRAdd>(IROperand(T1), IROperand(T2), IROperand(T1)));
    // Load inheritance table address into T0
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T0), IROperand(T1), IROperand(0)));

    // Prepare and sort branches by inheritance depth
    std::vector<branch_class*> case_branches;
    for(int i = cases->first(); cases->more(i); i = cases->next(i))
        case_branches.push_back(static_cast<branch_class*>(cases->nth(i)));

    std::sort(case_branches.begin(), case_branches.end(), [cgen_node](branch_class* first, branch_class* second) {
        CgenNodeP first_branch_type = cgen_node->get_symbol_table()->lookup(first->type_decl);
        CgenNodeP second_branch_type = cgen_node->get_symbol_table()->lookup(second->type_decl);
        return first_branch_type->get_inheritance_depth() > second_branch_type->get_inheritance_depth();
    });

    int case_finished_label = label_index++;

    for(auto it = case_branches.cbegin(); it != case_branches.cend(); ++it)
    {
        int case_match_found_label = label_index++;
        int case_branch_loop_label = label_index++;
        int case_branch_exit_label = label_index++;

        branch_class* caseBranch = *it;

        // Load inheritance table address into T1
        tac_statements.push_back(std::make_unique<IRAdd>(IROperand(T1), IROperand(T0), IROperand(0)));
        // Load proto obj address into T2
        tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T2), IRLabelOperand(get_protobj_ref(caseBranch->type_decl)), IROperand(0)));
        // Load class tag for this branch type into T2
        tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T2), IROperand(T2), IROperand(0)));

        // Loop: check inheritance table for match
        tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(case_branch_loop_label)));
        tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T3), IROperand(T1), IROperand(0)));

        // If T3 == 1, exit (no match)
        tac_statements.push_back(std::make_unique<IRAssign>(IROperand(T4), 1));
        tac_statements.push_back(std::make_unique<IRRelOp>(IROperand(T3), IROperand(T4), IRRelOp::Kind::IR_EQ));
        tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T3), IROperand(T4), IRRelOp::Kind::IR_EQ), get_label_ref(case_branch_exit_label)));

        // If T3 == T2, match found
        tac_statements.push_back(std::make_unique<IRRelOp>(IROperand(T3), IROperand(T2), IRRelOp::Kind::IR_EQ));
        tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T3), IROperand(T2), IRRelOp::Kind::IR_EQ), get_label_ref(case_match_found_label)));

        // Increment T1 and loop
        tac_statements.push_back(std::make_unique<IRAdd>(IROperand(T1), IROperand(T1), IROperand(WORD_SIZE)));
        tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(case_branch_loop_label)));

        // Match found: evaluate branch
        tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(case_match_found_label)));
        tac_statements.push_back(std::make_unique<IRStore>(IROperand(FP), IROperand(ACC), IROperand(-1 * local_index)));
        formals_table.enterscope();
        int* fp_offset = new int;
        *fp_offset = local_index * -1;
        formals_table.addid(caseBranch->name->get_string(), fp_offset);
        // increment for the next local var
        local_index++;
        caseBranch->expr->emit_ir(tac_statements, cgen_node, formals_table, local_index);
        formals_table.exitscope();
        local_index--;
        tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(case_finished_label)));

        // Branch exit
        tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(case_branch_exit_label)));
    }

    tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(case_finished_label)));
    // If T3 == 1 after all branches, abort
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(T4), 1));
    tac_statements.push_back(std::make_unique<IRRelOp>(IROperand(T3), IROperand(T4), IRRelOp::Kind::IR_EQ));
    tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T3), IROperand(T4), IRRelOp::Kind::IR_EQ), "_case_abort"));
}

void typcase_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   // First emit code to generate the predicate
   expr->code(s, cgen_node, formals_table, local_index);

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

      // Push the result of the case predicate evaluation onto the stack so that the case body expression can access it
      emit_store(ACC, -1*local_index, FP, s);

      // code gen for the case statement branch
      formals_table.enterscope();

      // Keep track of the case binding
      int* fp_offset = new int;
      *fp_offset = local_index * -1;
      formals_table.addid(caseBranch->name->get_string(), fp_offset);
      
      // Increment local index for the new variable
      local_index++;
      caseBranch->expr->code(s, cgen_node, formals_table, local_index);
      formals_table.exitscope();
      // Decrease the local index since we have exited the case variable scope
      local_index--;

      // Jump out of the case expression
      emit_jump(case_finished_label, s);

      emit_label_def(case_branch_exit_label, s);
   }
   
   emit_label_def(case_finished_label, s);

   // If T3 is equal to 1 upon testing all of the branches it means there was no match
   emit_addiu(T4, ZERO, 1, s);
   emit_beq(T3, T4, "_case_abort", s);
}

void block_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   for(int i = body->first(); body->more(i); i = body->next(i))
   {
      body->nth(i)->emit_ir(tac_statements, cgen_node, formals_table, local_index);
   }
}

void block_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
   for(int i = body->first(); body->more(i); i = body->next(i))
   {
      body->nth(i)->code(s, cgen_node, formals_table, local_index);
   }
}

void let_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    // Enter a new scope for the let variable
    formals_table.enterscope();

    // Evaluate the initializer expression (if any)
    if (dynamic_cast<no_expr_class*>(init) == nullptr) {
        init->emit_ir(tac_statements, cgen_node, formals_table, local_index);
        // Store the result of the initializer in the new variable slot
        tac_statements.push_back(std::make_unique<IRStore>(IROperand(FP), IROperand(ACC), IROperand(-1 * local_index)));
    } else {
        // If no initializer, assign default value based on type
        if (type_decl == Int || type_decl == Bool) {
            tac_statements.push_back(std::make_unique<IRAssign>(IROperand(ACC), 0));
        } else if (type_decl == Str) {
            tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(STRCONST_PREFIX "empty_str"), IROperand(0)));
        } else {
            tac_statements.push_back(std::make_unique<IRAssign>(IROperand(ACC), 0)); // void
        }
        tac_statements.push_back(std::make_unique<IRStore>(IROperand(FP), IROperand(ACC), IROperand(-1 * local_index)));
    }

    // Add the let variable to the symbol table
    int* fp_offset = new int;
    *fp_offset = local_index * -1;
    formals_table.addid(identifier->get_string(), fp_offset);

    // Increment local index for the new variable
    local_index++;

    // Emit IR for the body of the let expression
    body->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Exit the scope and decrement local index
    formals_table.exitscope();
    local_index--;
}

void let_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const {
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
      init->code(s, cgen_node, formals_table, local_index);
   }

   // Push the result of the copy onto the stack
   emit_store(ACC, -1 * local_index, FP, s);

   formals_table.enterscope();

   int* fp_offset = new int;
   *fp_offset = local_index * -1;
   formals_table.addid(identifier->get_string(), fp_offset);

   // Increment local index for the new variable
   local_index++;

   // Then evaluate the body of the let
   body->code(s, cgen_node, formals_table, local_index);

   formals_table.exitscope();

   // Decrease the local index since we have exited the let variable scope
   local_index--;
}

void emit_binary_op_prefix(Expression lhs, Expression rhs, ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int> formals_table,int& local_index)
{
  // This function assumes that Int objects will be present in register ACC after evaluating both lhs and rhs expressions

  lhs->code(s, cgen_node, formals_table, local_index);

  // push lhs result to stack
  emit_store(ACC, 0, SP, s);
  // bump the stack pointer
  emit_stack_size_push(1, s);

  rhs->code(s, cgen_node, formals_table, local_index);

  // load the memory address of the object into T2
  emit_load(T2, 1, SP, s);

  // load the int result from the lhs into T1
  emit_load(T1, 3, T2, s);

  // Load the int result from the rhs into T2
  emit_load(T2, 3, ACC, s);

  //T1 now stores the lhs int operand and T2 now stores the rhs int operand-
}

void emit_binary_op_prefix_ir(IRStatements& tac_statements, Expression lhs, Expression rhs, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index)
{
    // Evaluate lhs and push result to stack
    lhs->emit_ir(tac_statements, cgen_node, formals_table, local_index);
    tac_statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(ACC), IROperand(0)));
    append_ir_stack_size_push(tac_statements, 1);

    // Evaluate rhs
    rhs->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Load address of lhs result into T2
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T2), IROperand(SP), IROperand(1)));

    // Load int result from lhs into T1
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T1), IROperand(T2), IROperand(3)));

    // Load int result from rhs into T2
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T2), IROperand(ACC), IROperand(3)));
}

void emit_object_allocation(Symbol return_type, ostream &s)
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

void emit_object_allocation_ir(IRStatements& tac_statements, Symbol return_type)
{
    // Load the proto object address into ACC
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_protobj_ref(return_type)), IROperand(0)));
    // Copy the object onto the heap
    append_ir_object_copy(tac_statements);
    // Store the result in T2 into the object stored at ACC
    tac_statements.push_back(std::make_unique<IRStore>(IROperand(ACC), IROperand(T2), IROperand(3)));
}

// todo: Why do the results of expressions get allocated on the heap? How does that make any sense?
// Why couldn't we allocate the result of expressions on the stack?
// each time we code an expression (with the exception of dispatch since we put formals on the stack) the stack pointer is returned to its previous starting location
// this is in invariant that we can't break
// These allocations on the heap though *can* be stored in temp registers if we can figure out what temp registers are available,
// todo: take a look at this when implementing register allocation

void emit_binary_op_suffix(Symbol return_type, ostream &s)
{
  // For bools we don't need to allocate a new object
  if (return_type == Int) emit_object_allocation(return_type, s);

  // return the stack pointer to its previous value
  emit_stack_size_pop(1, s);
}

void emit_binary_op_suffix_ir(IRStatements& tac_statements, Symbol return_type)
{
    // For Int, allocate a new object
    if (return_type == Int)
        emit_object_allocation_ir(tac_statements, return_type);

    // Return the stack pointer to its previous value
    append_ir_stack_size_pop(tac_statements, 1);
}

void plus_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    // add T1 + T2, result in T2
    tac_statements.push_back(std::make_unique<IRAdd>(IROperand(T2), IROperand(T1), IROperand(T2)));

    emit_binary_op_suffix_ir(tac_statements, Int);
}

void plus_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);

  // add T1 + T2
  emit_add(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void sub_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    // sub T1 - T2, result in T2
    tac_statements.push_back(std::make_unique<IRSub>(IROperand(T2), IROperand(T1), IROperand(T2)));

    emit_binary_op_suffix_ir(tac_statements, Int);
}

void sub_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);

  // sub T1 - T2
  emit_sub(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void mul_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    // mul T1 * T2, result in T2
    tac_statements.push_back(std::make_unique<IRMul>(IROperand(T2), IROperand(T1), IROperand(T2)));

    emit_binary_op_suffix_ir(tac_statements, Int);
}

void mul_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);

  // mul T1 * T2
  emit_mul(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void divide_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    // div T1 / T2, result in T2
    tac_statements.push_back(std::make_unique<IRDiv>(IROperand(T2), IROperand(T1), IROperand(T2)));

    emit_binary_op_suffix_ir(tac_statements, Int);
}

void divide_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);

  // mul T1 / T2
  emit_div(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void neg_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    e1->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Load the int value into ACC
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IROperand(ACC), IROperand(3)));

    // Load -1 into T1
    tac_statements.push_back(std::make_unique<IRAssign>(IROperand(T1), -1));

    // Negate the int value by multiplying by -1, result in T2
    tac_statements.push_back(std::make_unique<IRMul>(IROperand(T2), IROperand(ACC), IROperand(T1)));

    // Create a new Int object on the heap and store the value of T2 in it
    emit_object_allocation_ir(tac_statements, Int);
}

void neg_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
    e1->code(s, cgen_node, formals_table, local_index);

    // Load the int value into ACC
    emit_load(ACC, 3, ACC, s);

    // Load -1 into T1
    emit_load_imm(T1, -1, s);

    // Negate the int value by multiplying by -1
    emit_mul(T2, ACC, T1, s);

    // Create a new Int object on the heap and store the value of T2 in it
    emit_object_allocation(Int, s);
}

void lt_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    int true_branch_label = label_index++;
    int false_branch_label = label_index++;

    // If T1 < T2, branch to true_branch_label
    tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T1), IROperand(T2), IRRelOp::Kind::IR_LT), get_label_ref(true_branch_label)));

    // False branch: load BoolConst(0) into ACC and jump to false_branch_label
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(0))), IROperand(0)));
    tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(false_branch_label)));

    // True branch: load BoolConst(1) into ACC
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(true_branch_label)));
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));

    // False branch label
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(false_branch_label)));

    emit_binary_op_suffix_ir(tac_statements, Bool);
}

void lt_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
   emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);
   int true_branch_label = label_index++;
   int false_branch_label = label_index++;

   emit_blt(T1, T2, true_branch_label, s);
   emit_load_bool(ACC, BoolConst(0), s);
   emit_jump(false_branch_label, s);
   emit_label_def(true_branch_label, s);
   emit_load_bool(ACC, BoolConst(1), s);
   emit_label_def(false_branch_label, s);

   emit_binary_op_suffix(Bool, s);
}

void leq_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    emit_binary_op_prefix_ir(tac_statements, e1, e2, cgen_node, formals_table, local_index);

    int true_branch_label = label_index++;
    int false_branch_label = label_index++;

    // If T1 <= T2, branch to true_branch_label
    tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T1), IROperand(T2), IRRelOp::Kind::IR_LEQ), get_label_ref(true_branch_label)));

    // False branch: load BoolConst(0) into ACC and jump to false_branch_label
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(0))), IROperand(0)));
    tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(false_branch_label)));

    // True branch: load BoolConst(1) into ACC
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(true_branch_label)));
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));

    // False branch label
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(false_branch_label)));

    emit_binary_op_suffix_ir(tac_statements, Bool);
}

void leq_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
   emit_binary_op_prefix(e1, e2, s, cgen_node, formals_table, local_index);
   int true_branch_label = label_index++;
   int false_branch_label = label_index++;

   emit_bleq(T1, T2, true_branch_label, s);
   emit_load_bool(ACC, BoolConst(0), s);
   emit_jump(false_branch_label, s);
   emit_label_def(true_branch_label, s);
   emit_load_bool(ACC, BoolConst(1), s);
   emit_label_def(false_branch_label, s);

   emit_binary_op_suffix(Bool, s);
}

void eq_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    e1->emit_ir(tac_statements, cgen_node, formals_table, local_index);
    // Push result of LHS onto the stack
    tac_statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(ACC), IROperand(0)));
    append_ir_stack_size_push(tac_statements, 1);

    e2->emit_ir(tac_statements, cgen_node, formals_table, local_index);
    // move RHS into T2
    tac_statements.push_back(std::make_unique<IRMove>(IROperand(T2), IROperand(ACC)));
    // load LHS into T1
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(T1), IROperand(SP), IROperand(1)));

    // True branch if pointers are equal
    int pointers_are_equal = label_index++;
    tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T2), IROperand(T1), IRRelOp::Kind::IR_EQ), get_label_ref(pointers_are_equal)));

    // Pointers not equal so continue with equality test
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(A1), IRLabelOperand(get_bool_const_ref(BoolConst(0))), IROperand(0)));
    tac_statements.push_back(std::make_unique<IRLabelJumpAndLink>("equality_test"));

    // True branch: load BoolConst(1) into ACC
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(pointers_are_equal)));
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));

    // Restore the stack pointer
    append_ir_stack_size_pop(tac_statements, 1);
}

void eq_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  e1->code(s, cgen_node, formals_table, local_index);
  // Push result of LHS onto the stack
  emit_store(ACC, 0, SP, s);
  emit_stack_size_push(1, s);

  e2->code(s, cgen_node, formals_table, local_index);
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
  emit_jal("equality_test", s);

  emit_label_def(pointers_are_equal, s);
  // Restore the stack pointer
  emit_stack_size_pop(1, s);
}

void comp_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
    e1->emit_ir(tac_statements, cgen_node, formals_table, local_index);

    // Load the bool value into ACC
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IROperand(ACC), IROperand(3)));

    // Invert the bool: if ACC < 1 set ACC to 1, else set to 0
    tac_statements.push_back(std::make_unique<IRRelOp>(IROperand(T2), IROperand(ACC), IROperand(1), IRRelOp::Kind::IR_LT));

    int zero_label = label_index++;
    int one_label = label_index++;

    tac_statements.push_back(std::make_unique<IRIfJump>(IRRelOp(IROperand(T2), IROperand(0), IRRelOp::Kind::IR_EQ), get_label_ref(zero_label)));
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));
    tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(one_label)));
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(zero_label)));
    tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(0))), IROperand(0)));
    tac_statements.push_back(std::make_unique<IRLabel>(get_label_def(one_label)));
}

// Note: this is actually the not operator. No idea why it is called comp
void comp_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
   // this produces a bool value in acc
   e1->code(s, cgen_node, formals_table, local_index);

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

void int_const_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int> &locals, int &local_index) const
{
   IntEntry* i = inttable.lookup_string(token->get_string());
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_int_const_ref(i)), IROperand(0)));
}

void int_const_class::code(ostream& s, CgenNodeP cgen_node, SymbolTable<std::string, int>&, int&) const
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int> &locals, int &local_index) const
{
   StringEntry* str = stringtable.lookup_string(token->get_string());
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_str_const_ref(str)), IROperand(0)));
}

void string_const_class::code(ostream& s, CgenNodeP cgen_node, SymbolTable<std::string, int>&, int&) const
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int> &locals, int &local_index) const
{
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(val))), IROperand(0)));
}

void bool_const_class::code(ostream& s, CgenNodeP cgen_node,  SymbolTable<std::string, int>&, int&) const
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
}

void new__class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>&,int& local_index) const
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
      emit_stack_size_push(1, s);
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
      emit_stack_size_pop(1, s);

      // jump to init method
      emit_jalr(T1, s);
   }
   else
   {
      // Invoke initialization method on the object that we just allocated
      std::stringstream init_method_ref;
      emit_init_ref(type_name, init_method_ref);
      emit_jal(init_method_ref.str().c_str(), s);
   }
}

void isvoid_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   e1->emit_ir(tac_statements, cgen_node, formals_table, local_index);

   int zero_label = label_index++;
   int one_label = label_index++;

   std::stringstream one_label_ref;
   emit_label_ref(one_label, one_label_ref);
   std::stringstream one_label_def;
   emit_label_def(one_label, one_label_def);

   IRRelOp eq_rel_op = IRRelOp(IROperand(ACC), IROperand(ZERO), IRRelOp::Kind::IR_EQ);
   tac_statements.push_back(std::make_unique<IRIfJump>(eq_rel_op, get_label_ref(zero_label)));
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(0))), IROperand(0)));
   tac_statements.push_back(std::make_unique<IRLabelJump>(get_label_ref(one_label)));
   tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(zero_label)));
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IRLabelOperand(get_bool_const_ref(BoolConst(1))), IROperand(0)));
   tac_statements.push_back(std::make_unique<IRLabel>(get_label_ref(one_label)));
}

void isvoid_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  e1->code(s, cgen_node, formals_table, local_index);

  int zero_label = label_index++;
  int one_label = label_index++;

  emit_beq(ACC, ZERO, zero_label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_jump(one_label, s);
  emit_label_def(zero_label, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_label_def(one_label, s);
}

void no_expr_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IROperand(0), IROperand(0)));
}

void no_expr_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
  // Load void into ACC for no_expr
  emit_load_imm(ACC, 0, s);
}

void object_class::emit_ir(IRStatements& tac_statements, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table, int& local_index) const
{
   if (name == self) 
   {
      tac_statements.push_back(std::make_unique<IRMove>(IROperand(ACC), IROperand(SELF)));
   }
   else if (formals_table.lookup(name->get_string()) != nullptr)
   {
      int* fp_offset = formals_table.lookup(name->get_string());
      tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IROperand(FP), *fp_offset));
   } 
   else
   {
      // else look for a matching attribute in the class
      int attribute_location = cgen_node->get_attribute_location(name);
      assert(attribute_location != -1);
      tac_statements.push_back(std::make_unique<IRLoad>(IROperand(ACC), IROperand(SELF), DEFAULT_OBJFIELDS + attribute_location));
   }
}

void object_class::code(ostream &s, CgenNodeP cgen_node, SymbolTable<std::string, int>& formals_table,int& local_index) const
{
   if (name == self) 
   {
      // Note: $s0 always refers to SELF
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
      emit_load(ACC, DEFAULT_OBJFIELDS + attribute_location, SELF, s);
   }
}


//
// Polymorphic virtual functions for each nodes to define.
//

int branch_class::get_number_of_locals() const {
  return 1 + expr->get_number_of_locals();
}

int method_class::get_number_of_locals() const {
  // Formals are not counted as locals.
  return expr->get_number_of_locals();
}

int assign_class::get_number_of_locals() const {
  return expr->get_number_of_locals();
}

int static_dispatch_class::get_number_of_locals() const {
  int number_of_locals = expr->get_number_of_locals();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    number_of_locals = std::max(number_of_locals, actual->nth(i)->get_number_of_locals());
  }
  return number_of_locals;
}

int dispatch_class::get_number_of_locals() const {
  int number_of_locals = expr->get_number_of_locals();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    number_of_locals = std::max(number_of_locals, actual->nth(i)->get_number_of_locals());
  }
  return number_of_locals;
}

int cond_class::get_number_of_locals() const {
  return std::max(pred->get_number_of_locals(),
                  std::max(then_exp->get_number_of_locals(),
                           else_exp->get_number_of_locals()));
}

int loop_class::get_number_of_locals() const {
  return std::max(pred->get_number_of_locals(), body->get_number_of_locals());
}

int typcase_class::get_number_of_locals() const {
  int number_of_locals = expr->get_number_of_locals();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    number_of_locals = std::max(number_of_locals, cases->nth(i)->get_number_of_locals());
  }
  return number_of_locals;
}

int block_class::get_number_of_locals() const {
  int number_of_locals = 0;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    number_of_locals = std::max(number_of_locals, body->nth(i)->get_number_of_locals());
  }
  return number_of_locals;
}

int let_class::get_number_of_locals() const {
  return 1 + body->get_number_of_locals();
}

int plus_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int sub_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int mul_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int divide_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int neg_class::get_number_of_locals() const {
  return e1->get_number_of_locals();
}

int lt_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int eq_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int leq_class::get_number_of_locals() const {
  return std::max(e1->get_number_of_locals(), e2->get_number_of_locals());
}

int comp_class::get_number_of_locals() const {
  return e1->get_number_of_locals();
}

int int_const_class::get_number_of_locals() const {
  return 0;
}

int string_const_class::get_number_of_locals() const {
  return 0;
}

int bool_const_class::get_number_of_locals() const {
  return 0;
}

int new__class::get_number_of_locals() const {
  return 0;
}

int isvoid_class::get_number_of_locals() const {
  return e1->get_number_of_locals();
}

int no_expr_class::get_number_of_locals() const {
  return 0;
}

int object_class::get_number_of_locals() const {
  return 0;
}
