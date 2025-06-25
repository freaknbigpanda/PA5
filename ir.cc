#include <sstream>
#include "ir.h"
#include "emit.h"
#include "cgen.h"

void append_ir_stack_size_push(std::vector<IRStatement>& statements, int num_words)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(-1 & num_words * WORD_SIZE);

    statements.push_back(IRPlus(dst, l, r));
}

void append_ir_stack_size_pop(std::vector<IRStatement>& statements, int num_words)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(num_words * WORD_SIZE);

    statements.push_back(IRPlus(dst, l, r));
}

void append_ir_object_copy(std::vector<IRStatement> &statements)
{
    statements.push_back(IRStatement(IRLabelJumpAndLink("Object.copy")));
}

void append_ir_callee_saves(std::vector<IRStatement>& statements) 
{
    append_ir_stack_size_push(statements, CALLEE_SAVES_SIZE); 
    statements.push_back(IRStore(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(IRStore(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(IRStore(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));

    statements.push_back(IRPlus(IROperand(FP), IROperand(SP), IROperand(CALLEE_SAVES_SIZE * WORD_SIZE))); 
}

void append_ir_callee_restores(std::vector<IRStatement>& statements, int parameter_count)
{
    statements.push_back(IRLoad(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(IRLoad(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(IRLoad(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));
    append_ir_stack_size_pop(statements, 3 + parameter_count);

    statements.push_back(IRRegJump(RA));
}

std::string get_label_ref(int label_index)
{
    return std::string();
}

std::string get_label_def(int label_index)
{
    return std::string();
}

std::string get_bool_const_ref(const BoolConst& b)
{
    std::stringstream bool_ref;
    b.code_ref(bool_ref);
    return bool_ref.str();
}

std::string get_str_const_ref(StringEntry *str)
{
    std::stringstream str_ref;
    str->code_ref(str_ref);
    return str_ref.str();
}

std::string get_int_const_ref(IntEntry *i)
{
    std::stringstream int_ref;
    i->code_ref(int_ref);
    return int_ref.str();
}
