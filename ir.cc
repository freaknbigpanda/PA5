#include "ir.h"

void append_ir_stack_size_push(std::vector<IRStatement>& statements, int num_words, int& sp)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(-1 & num_words * WORD_SIZE);

    statements.push_back(IRPlus(dst, l, r));
    sp -= num_words;
}

void append_ir_stack_size_pop(std::vector<IRStatement>& statements, int num_words, int& sp)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(num_words * WORD_SIZE);

    statements.push_back(IRPlus(dst, l, r));
    sp += num_words;
}

void append_ir_method_prefix(std::vector<IRStatement>& statements, int parameter_count, int& sp) 
{
    // Grow the stack 12 bytes for 3 words worth of shit
    append_ir_stack_size_push(statements, 3, sp); // SP = -3
    // Preserve all of the registers we have to for a function call
    // Note: if we use more of the $sx registers we will need to add more instructions here and in the method_suffix function to save them

    statements.push_back(IRStore(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(IRStore(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(IRStore(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));

    statements.push_back(IRPlus(IROperand(FP), IROperand(SP), IROperand(12 + (WORD_SIZE * parameter_count)))); // FP now points to the first parameter
}

void append_ir_method_suffix(std::vector<IRStatement>& statements, int parameter_count)
{
    statements.push_back(IRLoad(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(IRLoad(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(IRLoad(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));
    int does_not_matter;
    append_ir_stack_size_pop(statements, 3 + parameter_count, does_not_matter);

    statements.push_back(IRRegJump(RA));
}