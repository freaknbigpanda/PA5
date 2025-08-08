#include <memory>
#include "ir.h"
#include "emit.h"
#include "cgen.h"

void append_ir_stack_size_push(IRStatements& statements, int num_words)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(-1 * num_words * WORD_SIZE);

    statements.push_back(std::make_unique<IRAdd>(dst, l, r));
}

void append_ir_stack_size_pop(IRStatements& statements, int num_words)
{
    IROperand dst = IROperand(SP);
    IROperand l = IROperand(SP);
    IROperand r = IROperand(num_words * WORD_SIZE);

    statements.push_back(std::make_unique<IRAdd>(dst, l, r));
}

void append_ir_object_copy(IRStatements& statements)
{
    statements.push_back(std::make_unique<IRLabelJumpAndLink>("Object.copy"));
}

void append_ir_callee_saves(IRStatements& statements) 
{
    append_ir_stack_size_push(statements, CALLEE_SAVES_SIZE); 
    statements.push_back(std::make_unique<IRStore>(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(std::make_unique<IRStore>(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(std::make_unique<IRStore>(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));

    statements.push_back(std::make_unique<IRAdd>(IROperand(FP), IROperand(SP), IROperand(CALLEE_SAVES_SIZE * WORD_SIZE))); 
}

void append_ir_callee_restores(IRStatements& statements, int parameter_count)
{
    statements.push_back(std::make_unique<IRLoad>(IROperand(FP), IROperand(SP), IROperand(3 * WORD_SIZE)));
    statements.push_back(std::make_unique<IRLoad>(IROperand(SELF), IROperand(SP), IROperand(2 * WORD_SIZE)));
    statements.push_back(std::make_unique<IRLoad>(IROperand(RA), IROperand(SP), IROperand(1 * WORD_SIZE)));
    append_ir_stack_size_pop(statements, 3 + parameter_count);

    statements.push_back(std::make_unique<IRRegJump>(RA));
}