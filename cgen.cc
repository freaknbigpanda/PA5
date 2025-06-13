
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
#include <algorithm>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
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
      << WORD << get_tag_for_name(Int) << endl;
  str << BOOLTAG << LABEL
      << WORD << get_tag_for_name(Bool) << endl;
  str << STRINGTAG << LABEL
      << WORD << get_tag_for_name(Str) << endl;
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

  stringtable.code_string_table(str,get_tag_for_name(Str));
  inttable.code_string_table(str,get_tag_for_name(Int));
  code_bools(get_tag_for_name(Bool));
}



CgenClassTable::CgenClassTable(Classes classes, ostream& s) : code_gen_classes(NULL) , str(s)
{
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
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    // Apparently for garbage collection, need -1 in object address -4 (see cool-runtime.pdf)
    str << WORD << "-1" << endl;

    CgenNode* current_node_ptr = (*it).second;
    emit_protobj_ref(current_node_ptr->name, str);
    str << ":" << endl;
    str << WORD << current_node_ptr->get_tag() << endl;
    str << WORD << current_node_ptr->get_size() << endl;
    str << WORD << current_node_ptr->name << DISPTAB_SUFFIX << endl;

    std::vector<AttrOwnerPair> attributes = current_node_ptr->get_attributes();
    for (auto it = attributes.cbegin(); it != attributes.cend(); ++it)
    {
      attr_class* attribute = (*it).first;

      str << WORD;
      Symbol attribute_type = attribute->type_decl;
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
  }
}

void CgenClassTable::code_class_names()
{
  str << CLASSNAMETAB << ":" << endl;
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    str << WORD;
    CgenNode* current_node_ptr = (*it).second;
    current_node_ptr->get_string_entry()->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_obj_table()
{
  str << CLASSOBJTAB << ":" << endl;

  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    str << WORD;
    CgenNode* current_node_ptr = (*it).second;
    emit_protobj_ref(current_node_ptr->name, str);
    str << endl;
    str << WORD;
    emit_init_ref(current_node_ptr->name, str);
    str << endl;
  }
}

void CgenClassTable::code_inheritance_table()
{
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    CgenNodeP current_parent = (*it).second;

    emit_inhertable_ref(current_parent->name, str);
    str << ":" << endl;

    while (current_parent != nullptr) 
    {
      str << WORD << current_parent->get_tag() << endl;
      current_parent = current_parent->get_parentnd();
    }
  }
}

void CgenClassTable::code_class_tag_table()
{
  str << CLASSTAGTAB << ":" << endl;
  int current_class_tag = -1;
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    auto current_node = (*it).second;

    if (current_class_tag == -1) current_class_tag = current_node->get_tag();
    else current_class_tag = current_class_tag + 1;

    // there is an assumption that the current class increases by one for each class
    if (current_class_tag != current_node->get_tag()) abort();

    str << WORD << current_node->name << INHERTAB_SUFFIX << endl;
  }
}

void CgenClassTable::code_dispatch_table()
{
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;

    emit_disptable_ref(current_node_ptr->name, str);
    str << ":" << endl;

    std::vector<MethodOwnerPair> methods = current_node_ptr->get_methods();
    for (auto it = methods.cbegin(); it != methods.cend(); ++it)
    {
      str << WORD;
      emit_method_ref((*it).second, (*it).first->name, str);
      str << endl;
      int method_index = it - methods.begin();
      current_node_ptr->set_method_location((*it).first->name, method_index);
    }
  }
}

// todo: Make sure to test initialization of attributes that reference other attributes of the same class
void CgenClassTable::code_object_initializers()
{
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;
    emit_init_ref(current_node_ptr->name, str);
    str << ":" << endl;
    
    int sp = 0;
    emit_method_prefix(str, 0, sp);

    // todo: the cool runtime pdf mentions that ACC is saved for init methods.. not sure if I am doing that properly here or not

    // Save the value of self into register S0
    // note that init is *always* called after Object.copy which leaves a copy of the proto-object in ACC
    emit_move(SELF, ACC, str);
    

    if (current_node_ptr->name != Object)
    {
      std::stringstream init_ref;
      emit_init_ref(current_node_ptr->parent, init_ref);
      emit_jal(init_ref.str().c_str(), sp, 0, str);
    }

    // Initializing attributes
    std::vector<AttrOwnerPair> attributes = current_node_ptr->get_attributes();
    for (auto it = attributes.cbegin(); it != attributes.cend(); ++it)
    {
      attr_class* attribute = (*it).first;

      if ((*it).second != current_node_ptr->name) continue; // we don't need to initialize superclass attributes

      Expression init_expr = attribute->init;
      Symbol attr_type = attribute->type_decl;
      bool isBasic = (attr_type == Int || attr_type == Str || attr_type == Bool);

      // If the type is an Int or String and there is no expression, init with default proto-obj, otherwise emit code for the initialization expression
      if (dynamic_cast<no_expr_class*>(init_expr) != nullptr && isBasic)
      {
        // If we don't have an expression init the attribute to the value of the appropiate proto obj
        // Load the proto object address into ACC
        // Note that for non-basic types we just let them initialize to void
        emit_partial_load_address(ACC, str);
        emit_protobj_ref(attr_type, str);
        str << endl;

        emit_object_copy(str);
      }
      else
      {
        // Emit code for attribute initialization
        // Note: this will copy zero into ACC for no_expr_class
        SymbolTable<std::string, int> formal_table; // no method parameters for init methods so no need to populate the symbol table here with any offsets
        attribute->init->code(str, current_node_ptr, formal_table, sp, 0);
      }

      // Store the result of the attribute initialization in the correct location in the heap
      int attribute_index = current_node_ptr->get_attribute_location(attribute->name);
      emit_store(ACC, attribute_index + DEFAULT_OBJFIELDS, SELF, str);
    }

    // Restore the value of self back to register A0 before the method exits
    emit_move(ACC, SELF, str);

    emit_method_suffix(str, 0);
    if (cgen_debug) cout << "sp after coding init method for class " << current_node_ptr->get_name()->get_string() << " is " << sp << endl;
  }
}

void CgenClassTable::code_object_methods()
{
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    CgenNodeP current_node_ptr = (*it).second;
    // We don't need to generate methods for basic classes since it has already been done
    if (current_node_ptr->basic()) continue;

    std::vector<MethodOwnerPair> methods = current_node_ptr->get_methods();
    for (auto it = methods.cbegin(); it != methods.cend(); ++it)
    {
      // We don't need to emit method definitions for methods that are previously defined by a parent class
      if ((*it).second != current_node_ptr->name) continue;

      method_class* method = (*it).first;
      emit_method_ref(current_node_ptr->name, method->name, str);
      str << ":" << endl;
      
      Formals parameters = method->formals;
      // emit code for the method body
      int sp = 0; // used to record the position of the stack pointer so we can find local variables
      emit_method_prefix(str, parameters->len(), sp);

      // Save the value of self, which is stored in ACC on method entry, into register S0
      emit_move(SELF, ACC, str);

      // Contains frame pointer offsets for both local variables and parameters
      SymbolTable<std::string, int> symbol_table;
      symbol_table.enterscope();

      // Add formal FP offset for all of the formal identifiers (aka method parameters)
      for(int i = method->formals->first(); method->formals->more(i); i = method->formals->next(i))
      {
        // All of the formals will have already been pushed onto the stack by the dispatch call so we just need to set up the offset for the frame pointer here
        int* fp_offset = new int;
        *fp_offset = i * -1;
        symbol_table.addid(method->formals->nth(i)->get_name()->get_string(), fp_offset);
      }

      method->expr->code(str, current_node_ptr, symbol_table, sp, parameters->len());

      emit_method_suffix(str, parameters->len());
      if (cgen_debug) cout << "sp after coding method " << method->name->get_string() << " for class " << current_node_ptr->get_name()->get_string() << " is " << sp << endl;

      symbol_table.exitscope();
    }
  }
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
  Symbol name = nd->name;

  if (probe(name))
  {
    abort();
    return; // todo: not sure when this would get hit
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  code_gen_classes = new List<CgenNode>(nd,code_gen_classes);
  addid(name,nd);

  // Add to the class tag to cgennode map
  cgen_nodes_for_tag[nd->get_tag()] = nd;

  // Add the class name to the tag map
  class_tag_for_name[nd->get_name()] = nd->get_tag();
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
  // Set proto-object sizes and collect attributes and methods in vectors for all of the CgenNodes
  for(auto it = cgen_nodes_for_tag.cbegin(); it != cgen_nodes_for_tag.cend(); ++it)
  {
    CgenNode* current_node_ptr = (*it).second;
    current_node_ptr->set_size_attributes_methods();
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
  CgenNode *parent_node = probe(nd->parent);
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

void CgenNode::set_size_attributes_methods()
{
  size = DEFAULT_OBJFIELDS; // base object has a size of at least 3 for the class tag, object size, and dispatch pointer

  // First construct the list of classes to traverse
  std::vector<CgenNodeP> classes;
  CgenNodeP parent = this;
  while (parent != nullptr) {
    classes.push_back(parent);
    parent = parent->parentnd;
    
    // Set the inheritance depth for this CgenNode, this is used later in the case statement code generation
    inheritance_depth++;
  }

  std::map<std::string, MethodOwnerPair> method_names;

  // Now interate the classes in reverse order since we want to list methods and attributes of the super classes first
  for (auto it = classes.rbegin(); it != classes.rend(); it++ )
  {
    CgenNodeP current_class = *it;
    Features features = current_class->features;

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
      Feature feature = features->nth(i);
      if (feature->is_attr())
      {
        size++;
        attr_class* attribute = static_cast<attr_class*>(feature);
        AttrOwnerPair attribute_owner_pair = { attribute, current_class->name };
        attributes.push_back(attribute_owner_pair);
        attribute_name_map[attribute->name] = attribute_owner_pair;
      }
      else
      {
        
        method_class* method = static_cast<method_class*>(feature);
        MethodOwnerPair method_owner_pair = { method, current_class->name };

        //If the method was defined in a baseclass we don't to replace the method definition with the super class one
        if (method_names.find(method->name->get_string()) == method_names.end())
        {
          methods.push_back(method_owner_pair);
        } 
        else
        {
          // This means we have found a super class definition that is overriden in a base class
          // In this case we don't want to push back a new method, we just want to replace the current entry in methods
          // The reason for this is because when doing dispatch codegen we assume that the offset in the dispatch table for a particular 
          // method name is the same for all subclasses regardless of method overrides
          auto it = std::find(methods.begin(), methods.end(), method_names[method->name->get_string()]);
          assert(it != methods.end());
          methods.erase(it);
          methods.insert(it, method_owner_pair);
        }

        method_names[method->name->get_string()] = method_owner_pair;
      }
    }
  }
}

int CgenNode::get_attribute_location(Symbol attribute_name)
{
  AttrOwnerPair attribute = attribute_name_map[attribute_name];
  if (attribute.first == nullptr) 
  {
    return -1;
  }
  else 
  {
    int index = std::find(attributes.cbegin(), attributes.cend(), attribute) - attributes.cbegin();
    return index;
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

  if (cgen_debug) cout << "coding dispatch table" << endl;
  code_dispatch_table();

  if (cgen_debug) cout << "coding inheritance table" << endl;
  code_inheritance_table();

  if (cgen_debug) cout << "coding inheritance table" << endl;
  code_class_tag_table(); 

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding object initializers" << endl;
  code_object_initializers();

  if (cgen_debug) cout << "coding object methods" << endl;
  code_object_methods();
}

int CgenClassTable::get_next_class_tag(Symbol class_name)
{
  return lastclasstag++;
}

int CgenClassTable::get_tag_for_name(Symbol class_name) const
{
  if (class_tag_for_name.find(class_name) != class_tag_for_name.end())
  {
    return class_tag_for_name.find(class_name)->second;
  }
  else
  {
    abort();
    return -1;
  }
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
  basic_status(bstatus),
  symbol_table(ct)
{
  string_entry = stringtable.add_string(name->get_string()); // Add class name to string table
  Symbol class_name = nd->get_name();
  if (class_name != prim_slot && class_name != No_class && class_name != SELF_TYPE)
  {
    // Note that the three classes above are not assigned a class tag because they play no role in generated assembly and thus don't need a class tag
    tag = ct->get_next_class_tag(nd->get_name());
  }
   
  // Note that we do not initialize the size variable here because we don't know the size of the proto-object until we build the inheritance graph
}
