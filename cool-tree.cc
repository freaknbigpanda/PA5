//////////////////////////////////////////////////////////
//
// file: cool-tree.cc
//
// This file defines the functions of each class
//
//////////////////////////////////////////////////////////


#include "tree.h"
#include "cool-tree.handcode.h"
#include "cool-tree.h"
#include "cgen.h"
#include "emit.h"
#include <sstream>

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
// This value is increased whenever we emit code to push a value onto the stack 
// and decreased whenever we emit code to pop a value from the stack
int current_stack_size = 0;
// To get the correct stack offset for a particular let binding do the following:
// stack_size - let_bindings[let symbol]
static std::map<Symbol, int> let_bindings;

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

void assign_class::code(ostream &s, CgenNodeP cgen_node) {
   get_expr()->code(s, cgen_node);
   int attribute_location = cgen_node->get_attribute_location(get_symbol_name());
   emit_store(ACC, 3 + attribute_location, SELF, s);
}

void static_dispatch_class::code(ostream &s, CgenNodeP cgen_node) {
}

void dispatch_class::code(ostream &s, CgenNodeP cgen_node) {
}

void cond_class::code(ostream &s, CgenNodeP cgen_node) {
}

void loop_class::code(ostream &s, CgenNodeP cgen_node) {
   int exit_label = label_index++;
   int loop_label = label_index++;
   int evaluate_pred_label = label_index++;

   // first emit the code to evaluate the predicate
   emit_label_def(evaluate_pred_label, s);
   pred->code(s, cgen_node);

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
   emit_jal("equality_test", s);

   emit_beq(ACC, ZERO, exit_label, s);

   emit_label_def(loop_label, s);
   body->code(s, cgen_node);
   emit_jump(evaluate_pred_label, s);

   emit_label_def(exit_label, s);
   // loop always returns void
   emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s, CgenNodeP cgen_node) {
}

void block_class::code(ostream &s, CgenNodeP cgen_node) {
   for(int i = body->first(); body->more(i); i = body->next(i))
   {
      body->nth(i)->code(s, cgen_node);
   }
}

void let_class::code(ostream &s, CgenNodeP cgen_node) {
   Expression let_init = get_let_init();

   if (dynamic_cast<no_expr_class*>(let_init) == nullptr)
   {
      // If we have an init expr then evaluate the rhs
      let_init->code(s, cgen_node);
   }
   else
   {
      // If not then load the default proto-obj for this object
      emit_partial_load_address(ACC, s);
      emit_protobj_ref(get_let_type_decl(), s);
      s << endl;
   }

   // Now copy this object into a new location on the heap
   emit_jal("Object.copy", s);

   // Push the result of the copy onto the stack
   emit_store(ACC, 0, SP, s);

   // Keep track of the let binding
   let_bindings[get_let_id()] = current_stack_size;

   // bump the stack pointer
   emit_stack_size_push(1, s);

   // Then evaluate the body of the let
   get_let_body()->code(s, cgen_node);

   // Restore the stack pointer
   emit_stack_size_pop(1, s);
}

static void emit_binary_op_prefix(Expression lhs, Expression rhs, ostream &s, CgenNodeP cgen_node)
{
  // This function assumes that Int objects will be present in register ACC after evaluating both lhs and rhs expressions

  lhs->code(s, cgen_node);

  // push lhs result to stack
  emit_store(ACC, 0, SP, s);
  // bump the stack pointer
  emit_stack_size_push(1, s);

  rhs->code(s, cgen_node);

  // load the memory address of the object into T2
  emit_load(T2, 1, SP, s);

  // load the int result from the lhs into T1
  emit_load(T1, 3, T2, s);

  // Load the int result from the rhs into T2
  emit_load(T2, 3, ACC, s);

  //T1 now stores the lhs int operand and T2 now stores the rhs int operand-
}

static void emit_object_allocation(Symbol return_type, ostream &s)
{
  /* Create a new return object on the heap, populate its data field with the value in ACC, and populate ACC with a pointer to that object
      Note: Expects the raw result of the operand to be in register T2 */

  // Load the proto object address into ACC
  emit_partial_load_address(ACC, s);
  emit_protobj_ref(return_type, s);
  s << endl;

  // Create the object on the heap
  std::stringstream method_name;
  emit_method_ref(Object, copy, method_name);
  emit_jal(method_name.str().c_str(), s);

  // Store the result in T2 into the object stored at ACC
  emit_store(T2, 3, ACC, s);
}

// todo: Why do the results of expressions get allocated on the heap? How does that make any sense?
// Why couldn't we allocate the result of expressions on the stack?

static void emit_binary_op_suffix(Symbol return_type, ostream &s)
{
  // For bools we don't need to allocate a new object
  if (return_type == Int) emit_object_allocation(return_type, s);

  // return the stack pointer to its previous value
  emit_stack_size_pop(1, s);
}

void plus_class::code(ostream &s, CgenNodeP cgen_node)
{
  emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);

  // add T1 + T2
  emit_add(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void sub_class::code(ostream &s, CgenNodeP cgen_node)
{
  emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);

  // sub T1 - T2
  emit_sub(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void mul_class::code(ostream &s, CgenNodeP cgen_node)
{
  emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);

  // mul T1 * T2
  emit_mul(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void divide_class::code(ostream &s, CgenNodeP cgen_node)
{
  emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);

  // mul T1 / T2
  emit_div(T2, T1, T2, s);

  emit_binary_op_suffix(Int, s);
}

void neg_class::code(ostream &s, CgenNodeP cgen_node)
{
    get_rhs()->code(s, cgen_node);

    // Load the int value into ACC
    emit_load(ACC, 3, ACC, s);

    // Load -1 into T1
    emit_load_imm(T1, -1, s);

    // Negate the int value by multiplying by -1
    emit_mul(T2, ACC, T1, s);

    // Create a new Int object on the heap and store the value of T2 in it
    emit_object_allocation(Int, s);
}

void lt_class::code(ostream &s, CgenNodeP cgen_node)
{
   emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);
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

void leq_class::code(ostream &s, CgenNodeP cgen_node)
{
   emit_binary_op_prefix(get_lhs(), get_rhs(), s, cgen_node);
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

void eq_class::code(ostream &s, CgenNodeP cgen_node)
{
  get_lhs()->code(s, cgen_node);
  // Push result of LHS onto the stack
  emit_store(ACC, 0, SP, s);
  emit_stack_size_push(1, s);

  get_rhs()->code(s, cgen_node);
  // move RHS into T2
  emit_move(T2, ACC, s);
  // load LHS into T1
  emit_load(T1, 1, SP, s);

  // Value returned in ACC if true
  emit_load_bool(ACC, BoolConst(1), s);

  // Value return in ACC if false
  emit_load_bool(A1, BoolConst(0), s);

  // Jump and link to the compare method
  emit_jal("equality_test", s);

  // Restore the stack pointer
  emit_stack_size_pop(1, s);
}

// Note: this is actually the not operator. No idea why it is called comp
void comp_class::code(ostream &s, CgenNodeP cgen_node)
{
   int false_label = label_index++;
   int true_label = label_index++;
   // this produces a bool value in acc
   get_rhs()->code(s, cgen_node);

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

void int_const_class::code(ostream& s, CgenNodeP cgen_node)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenNodeP cgen_node)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenNodeP cgen_node)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenNodeP cgen_node)
{
  // todo: implement support for SELF_TYPE

  // Load the correct proto-object into ACC
  emit_partial_load_address(ACC, s);
  emit_protobj_ref(get_type_name(), s);
  s << endl;

  // Copy the proto-object to the heap
  // todo: don't really like this since if the method name changed this would break
  emit_jal("Object.copy", s);

  // Invoke initialization method on the object that we just allocated
  std::stringstream init_method_ref;
  emit_init_ref(get_type_name(), init_method_ref);
  emit_jal(init_method_ref.str().c_str(), s);
}

void isvoid_class::code(ostream &s, CgenNodeP cgen_node)
{
  get_rhs()->code(s, cgen_node);

  int zero_label = label_index++;
  int one_label = label_index++;

  emit_beq(ACC, ZERO, zero_label, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_jump(one_label, s);
  emit_label_def(zero_label, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_label_def(one_label, s);
}

void no_expr_class::code(ostream &s, CgenNodeP cgen_node)
{
  // Load void into ACC for no_expr
  emit_load_imm(ACC, 0, s);
}

void object_class::code(ostream &s, CgenNodeP cgen_node)
{
   // first check to see if there are any let variables with the given name
   if (let_bindings.find(get_name()) != let_bindings.end())
   {
      // if there are load the address into acc and return that
      emit_load(ACC, current_stack_size - let_bindings[get_name()], SP, s);
   }
   else
   {
      // else look for a matching attribute
      int attribute_location = cgen_node->get_attribute_location(get_name());
      emit_load(ACC, 3 + attribute_location, SELF, s);
   }
}
