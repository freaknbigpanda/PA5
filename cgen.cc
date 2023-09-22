
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <vector>
#include <sstream>
#include <set>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}

// enum class SRegisters: uint8_t
// {
//   S0 = 0b00000001,
//   S1 = 0b00000010,
//   S2 = 0b00000100,
//   S3 = 0b00001000,
//   S4 = 0b00010000,
//   S5 = 0b00100000,
//   S6 = 0b01000000,
//   S7 = 0b10000000
// };

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

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

// static void emit_callee_activation_record(std::set<SRegisters> used_s_registers, uint8_t number_of_parameters, ostream& s)
// {
//   static const std::map<SRegisters, std::string> registers_to_names = {{SRegisters::S0, "$s0"}, {SRegisters::S1, "$s1"}, 
//   {SRegisters::S2, "$s2"}, {SRegisters::S3, "$s3"}, {SRegisters::S4, "$s4"}, {SRegisters::S5, "$s5"}, 
//   {SRegisters::S6, "$s6"}, {SRegisters::S7, "$s7"}};

//   // Restore the fp and ra registers to their state before we entered the method
//   int offset = 0;
//   emit_load(FP, offset++, SP, s);
//   emit_load(RA, offset++, SP, s);

//   // todo: might need to add this back later
//   // for (auto it = used_s_registers.begin(); it != used_s_registers.end(); ++it)
//   // {
//   //   emit_load(registers_to_names.find(*it)->second.c_str(), offset++, SP, s);
//   // }

//   // subtract the stack pointer so that it is where it was before we entered the method, including the number of parameters that the caller pushed onto to the stack
//   emit_addiu(SP, SP, (used_s_registers.size() + 2 + number_of_parameters) * WORD_SIZE, s);

// }

static void emit_store(const char *source_reg, int offset,const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg,const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_callee_activation_record(/*std::set<SRegisters> used_s_registers,*/ Expression method_body, const int number_of_parameters, ostream& s)
{
  // static const std::map<SRegisters, std::string> registers_to_names = {{SRegisters::S0, "$s0"}, {SRegisters::S1, "$s1"}, 
  // {SRegisters::S2, "$s2"}, {SRegisters::S3, "$s3"}, {SRegisters::S4, "$s4"}, {SRegisters::S5, "$s5"}, 
  // {SRegisters::S6, "$s6"}, {SRegisters::S7, "$s7"}};

  // // grow the stack pointer for all of the used s registers + fp + ra
  // emit_addiu(SP, SP, (-1 * used_s_registers.size() + 2) * WORD_SIZE, s);

  // push all the registers to the stack
  // int offset = 0;
  // emit_store(RA, offset++, SP, s);

  // setup the frame pointer to point to what?

  // todo: might need to add this back later
  // for (auto it = used_s_registers.begin(); it != used_s_registers.end(); ++it)
  // {
  //   emit_store(registers_to_names.find(*it)->second.c_str(), offset++, SP, s);
  // }

  // setup a new frame pointer at the current stack pointer
  emit_move(FP, SP, s);
  
  // store ra register on the stack because codegen for the method body may trample it
  emit_store(RA, 0, SP, s);
  emit_addiu(SP, SP, WORD_SIZE, s);

  // store register S0 on the stack because apparently this is required
  // todo: figure out why
  // emit_store(SELF, 0, SP, s);
  // emit_addiu(SP, SP, WORD_SIZE, s);
  
  // move the value of SELF into ACC for function body execution
  // emit_move(ACC, SELF, s);

  // emit code for the method body
  method_body->code(s);

  // todo: I don't see why I would want to do this, ACC will contain the function result.
  // emit_move(SELF, ACC, s); 

  // restore s0/SELF register
  emit_load(SELF, 1, SP, s);

  // restore ra register
  emit_load(RA, 2, SP, s);
  
  // restore old sp from before the method was called, number of parameters + 1 for ra, 1 for fp, and 1 for s0/SELF
  emit_addiu(SP, SP, (number_of_parameters + 2) * WORD_SIZE, s);

  // restore old frame pointer so that the function that called us will have the frame pointer back
  emit_load(FP, 0, SP, s);

  // jump to address in ra
  emit_return(s);
}

static void emit_caller_activation_record(Expressions parameters, const std::string& method_name, ostream& s)
{
  // save value of frame pointer so that it can be restored after function exit
  emit_store(FP, 0, SP, s);
  emit_addiu(SP, SP, -1 * WORD_SIZE, s);

  // save actual parameters in reverse order, the first parameter will be at the lowest address
  for(int i = parameters->first(); parameters->more(i); i = parameters->next(i))
  {
      parameters->nth(i)->code(s);
      emit_store(ACC, 0, SP, s);
      emit_addiu(SP, SP, -1 * WORD_SIZE, s);
  }

  // jal instruction to jump to the method
  emit_jal(method_name.c_str(), s);
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << "String" << DISPATCH;
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << "Int" << DISPATCH;
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << "Bool" << DISPATCH;
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  //todo: I really don't understand why we want to use ALIGN here, just don't get it really
  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}



CgenClassTable::CgenClassTable(Classes classes, ostream& s) : code_gen_classes(NULL) , str(s)
{
  stringclasstag = 4 /* Change to your String class tag here */;
  intclasstag =    2 /* Change to your Int class tag here */;
  boolclasstag =   3 /* Change to your Bool class tag here */;
  nonbasicclasstag = 5;

  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
  exitscope();
}

void CgenClassTable::code_prototype_objects()
{
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;
    emit_protobj_ref(current_node_ptr->get_name(), str);
    str << ":" << endl;
    str << WORD << current_node_ptr->get_tag() << endl;
    str << WORD << current_node_ptr->get_size() << endl;
    str << WORD << current_node_ptr->get_name() << DISPTAB_SUFFIX << endl;

    // if (current_node_ptr->get_name() == Bool || current_node_ptr->get_name() == Str || current_node_ptr->get_name() == Int)
    // {
    //   //todo: no clue what this is for right now but coolc emits this for string bool and int proto objects
    //   // The cool runtime system document writes the following 
    //               /*For Int objects, the only attribute is the 32-bit value of the integer. For Bool objects, the only
    //           attribute is the 32-bit value 1 or 0, representing either true or false. The first attribute of String objects
    //           is an object pointer to an Int object representing the size of the string. The actual sequence of ASCII
    //           characters of the string starts at the second attribute (offset 16), terminates with a 0, and is then padded
    //           with 0’s to a word boundary*/
    //   // But I still don't understand why the proto-object wouldn't be using a int_const0
    //   // Don't really understand the relationship between proto objects and consts
    //   str << WORD << "0" << endl;
    // }

    Features features = current_node_ptr->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
      Feature feature = features->nth(i);
      if (feature->is_attr() == false) continue;

      attr_class* attribute = static_cast<attr_class*>(feature);

      str << WORD;
      Symbol attribute_type = attribute->get_declared_type();
      if (attribute_type == Int)
      {
        IntEntry* const_entry = inttable.lookup_string("0");
        const_entry->code_ref(str);
      } 
      else if (attribute_type == Bool)
      {
        falsebool.code_ref(str);
      }
      else if (attribute_type == Str)
      {
        StringEntry* const_entry = stringtable.lookup_string("");
        const_entry->code_ref(str);
      }
      else
      {
        // void for everything else
        str << "0";
      }
      str << endl;

      
    }

    if (current_node_ptr->get_name() != Main)
    {
      str << WORD << "-1" << endl;
    }
  }
}

void CgenClassTable::code_class_names()
{
  str << CLASSNAMETAB << endl;
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    str << WORD;
    CgenNode* current_node_ptr = (*it).second;
    current_node_ptr->get_string_entry()->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_obj_table()
{
  str << CLASSOBJTAB << endl;
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    str << WORD;
    CgenNode* current_node_ptr = (*it).second;
    emit_protobj_ref(current_node_ptr->get_name(), str);
    str << endl;
    str << WORD;
    emit_init_ref(current_node_ptr->get_name(), str);
    str << endl;
  }
}

void CgenClassTable::code_dispatch_table()
{
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;

    std::vector<std::pair<Symbol,Symbol>> methods;

    std::vector<CgenNodeP> inheritance_chain;

    // Building an inheritance chain so that we can interate over it backwards since we want to print references to the most parent object methods first (i.e. methods of Object first)
    CgenNodeP parent = current_node_ptr;
    while (parent != nullptr)
    {
      inheritance_chain.push_back(parent);
      parent = parent->get_parentnd();
    }

    for(auto it = inheritance_chain.crbegin(); it != inheritance_chain.crend(); ++it)
    {
      CgenNodeP current_cgen_node = *it;
      for (int i = current_cgen_node->features->first(); current_cgen_node->features->more(i); i = current_cgen_node->features->next(i))
      {
        Feature feature = current_cgen_node->features->nth(i);
        if (feature->is_attr()) continue; // only care about methods
        method_class* method_object = static_cast<method_class*>(feature);

        methods.push_back({current_cgen_node->get_name(), method_object->name});
      }
    }

    emit_disptable_ref(current_node_ptr->get_name(), str);
    str << endl;

    for (auto it = methods.cbegin(); it != methods.cend(); ++it)
    {
      str << WORD;
      emit_method_ref((*it).first, (*it).second, str);
      str << endl;
    }
    
  }
}

void CgenClassTable::code_object_initializers()
{
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;
    emit_init_ref(current_node_ptr->get_name(), str);
    str << endl;
    // Grow the stack 12 bytes for 3 words worth of shit
    emit_addiu(SP, SP, -12, str);
    // Preserve all of the registers we have to for a function call
    // Todo: the runtime system pdf mentions that s0-s7 are "The standard callee-saved registers on the MIPS architecture" so not sure if I need to save them all here or not
    // I should only need to save these registers if I use them
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    
    // the stack pointer now points to unused stack memory, set the FP to be 1 word before
    emit_addiu(FP, SP, 4, str);

    // Save the value of self into register S0
    emit_move(SELF, ACC, str);
    if (current_node_ptr->get_name() != Object) 
    {
      std::stringstream init_ref;
      emit_init_ref(current_node_ptr->get_parent(), init_ref);
      emit_jal(init_ref.str().c_str(), str);
    }

    // Initialize attributes here
    Features features = current_node_ptr->get_features();
    int attribute_index = 0;
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
      Feature feature = features->nth(i);
      if (features->nth(i)->is_attr() == false) continue;
      
      // Emit code for attribute initialization
      feature->get_expression()->code(str);

      // Store the result of the attribute initialization in the correct location in the heap
      emit_store(ACC, attribute_index + 3, SELF, str);
      attribute_index++;
    }

    // Restore the value of self back to register A0 before the method exits
    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);

    emit_return(str);
  }
}

void CgenClassTable::code_object_methods()
{

}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return; // todo: not sure when this would get hit 
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  code_gen_classes = new List<CgenNode>(nd,code_gen_classes);
  addid(name,nd);

  // Add to the class tag to cgennode map
  cgen_nodes_for_class[nd->get_tag()] = nd;
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = code_gen_classes; l; l = l->tl())
  {
      set_relations(l->hd());
  }

  // Now that the inheritance tree is built we can calculate the size of the objects
  // Set proto-object sizes for all of the CgenNodes
  for(auto it = cgen_nodes_for_class.cbegin(); it != cgen_nodes_for_class.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;
    current_node_ptr->calculate_size();
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::calculate_size()
{
  size = 3; // base object has a size of at least 3

  CgenNodeP current_parent = this;

  while (current_parent != nullptr) {
    for (int i = current_parent->features->first(); current_parent->features->more(i); i = current_parent->features->next(i))
    {
      if (current_parent->features->nth(i)->is_attr()) size++;
    }
    current_parent = current_parent->parentnd;
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding prototype objects" << endl;
  code_prototype_objects();

  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_names();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_obj_table();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_table();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding object initializers" << endl;
  code_object_initializers();

  if (cgen_debug) cout << "coding object methods" << endl;
  code_object_methods();
}

int CgenClassTable::get_next_class_tag(Symbol class_name)
{
  if (class_name == Bool) return boolclasstag;
  else if (class_name == Int) return intclasstag;
  else if (class_name == Str) return stringclasstag; 
  else return nonbasicclasstag++;
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   string_entry = stringtable.add_string(name->get_string()); // Add class name to string table
   tag = ct->get_next_class_tag(nd->get_name());

   // Note that we do not initialize the size variable here because we don't know the size of the proto-object until we build the inheritance graph
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

void assign_class::code(ostream &s) {
}

void static_dispatch_class::code(ostream &s) {
}

void dispatch_class::code(ostream &s) {
}

void cond_class::code(ostream &s) {
}

void loop_class::code(ostream &s) {
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) {
}

void let_class::code(ostream &s) {
}

void plus_class::code(ostream &s) {
}

void sub_class::code(ostream &s) {
}

void mul_class::code(ostream &s) {
}

void divide_class::code(ostream &s) {
}

void neg_class::code(ostream &s) {
}

void lt_class::code(ostream &s) {
}

void eq_class::code(ostream &s) {
}

void leq_class::code(ostream &s) {
}

void comp_class::code(ostream &s) {
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
}

void isvoid_class::code(ostream &s) {
}

void no_expr_class::code(ostream &s) {
}

void object_class::code(ostream &s) {
}


