#include "emit.h"
#include "cgen.h"
#include "cgen_gc.h"

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

void emit_store(const char *source_reg, int offset,const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

void emit_load_address(const char *dest_reg,const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

void emit_andi(const char *dest, const char *src1, int imm, ostream& s)
{ s << ANDI << dest << " " << src1 << " " << imm << endl; }

void emit_and(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ANDI << dest << " " << src1 << " " << src2 << endl; }

void emit_ori(const char *dest, const char *src1, int imm, ostream& s)
{ s << ORI << dest << " " << src1 << " " << imm << endl; }

void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

void emit_stack_size_push(int num_words, int& sp, ostream& s)
{ emit_addiu(SP, SP, -1 * num_words * WORD_SIZE, s); sp -= num_words; }

void emit_stack_size_pop(int num_words, int& sp, ostream& s)
{ emit_addiu(SP, SP, num_words * WORD_SIZE, s); sp += num_words; }

void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

void emit_slt(const char *cmp_result, const char *lhs, const char *rhs, ostream& s)
{ s << SLT << cmp_result << " " << lhs << " " << rhs << endl; }

void emit_slti(const char *cmp_result, const char *lhs, int imm, ostream& s)
{ s << SLTI << cmp_result << " " << lhs << " " << imm << endl; }

void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

void emit_return(ostream& s)
{ s << RET << endl; }

void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

void emit_inhertable_ref(Symbol sym, ostream& s)
{ s << sym << INHERTAB_SUFFIX; }

void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

void emit_jump(char *label, ostream &s)
{
  s << JUMP << label << endl;
}

void emit_jump(int label, ostream &s)
{
  s << JUMP;
  emit_label_ref(label,s);
  s << endl;
}

void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_beq(char *src1, char *src2, char* label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " " << label << endl;
}

void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

void emit_method_prefix(ostream &str, int parameter_count, int& sp) 
{
  // Grow the stack 12 bytes for 3 words worth of shit
  emit_stack_size_push(3, sp, str); // SP = -3
  // Preserve all of the registers we have to for a function call
  // Todo: the runtime system pdf mentions that s0-s7 are "The standard callee-saved registers on the MIPS architecture" so not sure if I need to save them all here or not
  // I should only need to save these registers if I use them
  emit_store(FP, 3, SP, str); // Store at SP = 0
  emit_store(SELF, 2, SP, str); // Store at SP = -1
  emit_store(RA, 1, SP, str); // Store at SP = -2

  emit_addiu(FP, SP, 12 + (WORD_SIZE * parameter_count), str); // FP now points to the first parameter
}

void emit_method_suffix(ostream &str, int parameter_count, int& sp)
{
   emit_load(FP, 3, SP, str);
   emit_load(SELF, 2, SP, str);
   emit_load(RA, 1, SP, str);
   emit_stack_size_pop(3 + parameter_count, sp, str);

   emit_return(str);
}
