#include <assert.h>
#include <stdio.h>
#include <map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;
using CgenNodeMap = std::map<int, CgenNodeP>;
using ClassNameToTagMap = std::map<Symbol, int>;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *code_gen_classes;
   ostream& str;
   int lastclasstag;
   bool is_tac_generated = false;
   CgenNodeMap cgen_nodes_for_tag;
   ClassNameToTagMap class_tag_for_name;

   // The following methods emit code for
   // constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_prototype_objects();
   void code_class_names();
   void code_obj_table();
   void code_inheritance_table();
   void code_class_tag_table();
   void code_dispatch_table();
   void code_object_initializers();
   void code_object_methods();

   // The following methods generate three address code IR representation for optimizations 

   void generate_tac_for_object_initializers();
   void generate_tac_for_object_methods();

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void generate_tac();
   void code();
   int get_next_class_tag(Symbol class_name);
   int get_tag_for_name(Symbol class_name) const;
   CgenNodeMap get_cgen_node_map() const { return cgen_nodes_for_tag; }
   CgenNodeP root();
};

using AttrOwnerPair = std::pair<attr_class*, Symbol>;
using MethodOwnerPair = std::pair<method_class*, Symbol>;

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int tag = -1;
   int size = -1;
   int inheritance_depth = -1;
   StringEntryP string_entry = nullptr;
   SymbolTable<Symbol,CgenNode>* symbol_table;

   std::vector<MethodOwnerPair> methods;
   std::map<Symbol, int> method_location_map;
   std::vector<AttrOwnerPair> attributes;
   std::map<Symbol, AttrOwnerPair> attribute_name_map;
   std::map<Symbol, std::vector<IRInstruction>> attribute_name_to_ir_map;
   std::map<Symbol, std::vector<IRInstruction>> method_name_to_ir_map;
   
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int get_size() { return size;}
   int get_tag() { return tag; }
   int get_inheritance_depth() { return inheritance_depth; }
   SymbolTable<Symbol,CgenNode>* get_symbol_table() { return symbol_table; }
   StringEntryP get_string_entry() { return string_entry; }
   void set_size_attributes_methods();
   int get_attribute_location(Symbol attribute_name);
   int get_attribute_location(const std::string& attribute_name);
   void set_attr_init_ir(Symbol attribute_name, const std::vector<IRInstruction>& ir) { attribute_name_to_ir_map[attribute_name] = ir; } 
   int get_method_location(Symbol method_name) { 
      return method_location_map.find(method_name) == method_location_map.end() ? -1 : method_location_map[method_name]; 
   }
   void set_method_location(Symbol method_name, int location) { method_location_map[method_name] = location; }
   void set_method_ir(Symbol method_name, const std::vector<IRInstruction>& ir) { method_name_to_ir_map[method_name] = ir; }
   void code_attr_init_from_ir(ostream &s, Symbol attribute_name, SymbolTable<std::string, int> formals_table, int& sp, int num_params);
   void code_method_from_ir(ostream &s, Symbol method_name, SymbolTable<std::string, int> formals_table, int& sp, int num_params);
   std::vector<MethodOwnerPair> get_methods() const { return methods; }
   std::vector<AttrOwnerPair> get_attributes() const { return attributes; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

