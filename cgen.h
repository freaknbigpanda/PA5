#include <assert.h>
#include <stdio.h>
#include <map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;
using CgenNodeMap = std::map<int, CgenNodeP>;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *code_gen_classes;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int nonbasicclasstag;
   CgenNodeMap cgen_nodes_for_class;

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
   void code_dispatch_table();
   void code_object_initializers();
   void code_object_methods();

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
   void code();
   int get_next_class_tag(Symbol class_name);
   CgenNodeP get_class_with_tag(int tag) const;
   CgenNodeMap get_cgen_node_map() const { return cgen_nodes_for_class; }
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
   StringEntryP string_entry = nullptr;

   std::vector<MethodOwnerPair> methods;
   std::map<Symbol, MethodOwnerPair> method_name_map;
   std::vector<AttrOwnerPair> attributes;
   std::map<Symbol, AttrOwnerPair> attribute_name_map;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int get_size() { return size; }
   int get_tag() { return tag; }
   StringEntryP get_string_entry() { return string_entry; }
   void set_size_attributes_methods();
   int get_attribute_location(Symbol attribute_name);
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

