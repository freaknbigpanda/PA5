///////////////////////////////////////////////////////////////////////
//
//  Assembly Code Naming Conventions:
//
//     Dispatch table            <classname>_dispTab
//     Method entry point        <classname>.<method>
//     Class init code           <classname>_init
//     Abort method entry        <classname>.<method>.Abort
//     Prototype object          <classname>_protObj
//     Integer constant          int_const<Symbol>
//     String constant           str_const<Symbol>
//
///////////////////////////////////////////////////////////////////////

#include "stringtab.h"

#define MAXINT  100000000    
#define WORD_SIZE    4
#define LOG_WORD_SIZE 2     // for logical shifts

// Global names
#define CLASSNAMETAB         "class_nameTab"
#define CLASSTAGTAB          "class_tagTab"
#define CLASSOBJTAB          "class_objTab"
#define INTTAG               "_int_tag"
#define BOOLTAG              "_bool_tag"
#define STRINGTAG            "_string_tag"
#define HEAP_START           "heap_start"

// Naming conventions

#define INHERTAB_SUFFIX      "_inherTab"
#define DISPTAB_SUFFIX       "_dispTab"
#define METHOD_SEP           "."
#define CLASSINIT_SUFFIX     "_init"
#define PROTOBJ_SUFFIX       "_protObj"
#define OBJECTPROTOBJ        "Object_protObj"
#define INTCONST_PREFIX      "int_const"
#define STRCONST_PREFIX      "str_const"
#define BOOLCONST_PREFIX     "bool_const"


#define EMPTYSLOT            0
#define LABEL                ":\n"

#define STRINGNAME (char *) "String"
#define INTNAME    (char *) "Int"
#define BOOLNAME   (char *) "Bool"
#define MAINNAME   (char *) "Main"

//
// information about object headers
//
#define DEFAULT_OBJFIELDS 3
#define TAG_OFFSET 0
#define SIZE_OFFSET 1
#define DISPTABLE_OFFSET 2

#define STRING_SLOTS      1
#define INT_SLOTS         1
#define BOOL_SLOTS        1

#define GLOBAL              "\t.globl\t"
#define ALIGN               "\t.align\t2\n"
#define WORD                "\t.word\t"
#define DISPATCH            "_dispTab"

//
// register names
//
#define ZERO "$zero"		// Zero register 
#define ACC  "$a0"		// Accumulator 
#define A1   "$a1"		// For arguments to prim funcs 
#define SELF "$s0"		// Ptr to self (callee saves) 
#define T0   "$t0"		// Temporary 1 
#define T1   "$t1"		// Temporary 1 
#define T2   "$t2"		// Temporary 2 
#define T3   "$t3"		// Temporary 3 
#define T4   "$t4"		// Temporary 4 
#define SP   "$sp"		// Stack pointer 
#define FP   "$fp"		// Frame pointer 
#define RA   "$ra"		// Return address 

//
// Opcodes
//
#define JUMP  "\tj\t"
#define JALR  "\tjalr\t"  
#define JAL   "\tjal\t"                 
#define RET   "\tjr\t$ra\t"

#define SW    "\tsw\t"
#define LW    "\tlw\t"
#define LI    "\tli\t"
#define LA    "\tla\t"

#define MOVE  "\tmove\t"
#define NEG   "\tneg\t"
#define ORI   "\tori\t"
#define ANDI   "\tandi\t"
#define ADD   "\tadd\t"
#define ADDI  "\taddi\t"
#define ADDU  "\taddu\t"
#define ADDIU "\taddiu\t"
#define DIV   "\tdiv\t"
#define MUL   "\tmul\t"
#define SUB   "\tsub\t"
#define SLL   "\tsll\t"
#define SLT   "\tslt\t"
#define SLTI   "\tslti\t"
#define BEQZ  "\tbeqz\t"
#define BRANCH   "\tb\t"
#define BEQ      "\tbeq\t"
#define BNE      "\tbne\t"
#define BLEQ     "\tble\t"
#define BLT      "\tblt\t"
#define BGT      "\tbgt\t"

class BoolConst;

void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s);
void emit_store(const char *source_reg, int offset,const char *dest_reg, ostream& s);
void emit_load_imm(const char *dest_reg, int val, ostream& s);
void emit_load_address(const char *dest_reg,const char *address, ostream& s);
void emit_partial_load_address(const char *dest_reg, ostream& s);
void emit_load_bool(const char *dest, const BoolConst& b, ostream& s);
void emit_load_string(char *dest, StringEntry *str, ostream& s);
void emit_load_int(char *dest, IntEntry *i, ostream& s);
void emit_move(const char *dest_reg, const char *source_reg, ostream& s);
void emit_neg(const char *dest, const char *src1, ostream& s);
void emit_add(const char *dest, const char *src1, const char *src2, ostream& s);
void emit_andi(const char *dest, const char *src1, int imm, ostream& s);
void emit_and(const char *dest, const char *src1, const char *src2, ostream& s);
void emit_ori(const char *dest, const char *src1, int imm, ostream& s);
void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s);
void emit_addiu(const char *dest, const char *src1, int imm, ostream& s);
void emit_stack_size_pop(int num_words, ostream &s);
void emit_stack_size_push(int num_words, ostream &s);
void emit_div(const char *dest, const char *src1, const char *src2, ostream &s);
void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s);
void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s);
void emit_sll(const char *dest, const char *src1, int num, ostream& s);
void emit_slt(const char *cmp_result, const char *lhs, const char *rhs, ostream& s);
void emit_slti(const char *cmp_result, const char *lhs, int imm, ostream& s);
void emit_jalr(const char *dest, ostream& s);
void emit_jal(const char *address,ostream &s);
void emit_return(ostream& s);
void emit_gc_assign(ostream& s);
void emit_disptable_ref(Symbol sym, ostream& s);
void emit_inhertable_ref(Symbol sym, ostream &s);
void emit_init_ref(Symbol sym, ostream &s);
void emit_label_ref(int l, ostream &s);
void emit_protobj_ref(Symbol sym, ostream& s);
void emit_method_ref(Symbol classname, Symbol methodname, ostream& s);
void emit_label_def(int l, ostream &s);
void emit_jump(char *label, ostream &s);
void emit_jump(int label, ostream &s);
void emit_beqz(char *source, int label, ostream &s);
void emit_beq(char *src1, char *src2, int label, ostream &s);
void emit_beq(char *src1, char *src2, char *label, ostream &s);
void emit_bne(char *src1, char *src2, int label, ostream &s);
void emit_bleq(char *src1, char *src2, int label, ostream &s);
void emit_blt(char *src1, char *src2, int label, ostream &s);
void emit_blti(char *src1, int imm, int label, ostream &s);
void emit_bgti(char *src1, int imm, int label, ostream &s);
void emit_branch(int l, ostream& s);
void emit_push(char *reg, ostream& str);
void emit_fetch_int(char *dest, char *source, ostream& s);
void emit_store_int(char *source, char *dest, ostream& s);
void emit_test_collector(ostream &s);
void emit_gc_check(char *source, ostream &s);

int emit_method_prefix(ostream &str);
void emit_method_suffix(ostream &str, int parameter_count);
