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
    statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(FP), IROperand(3)));
    statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(SELF), IROperand(2)));
    statements.push_back(std::make_unique<IRStore>(IROperand(SP), IROperand(RA), IROperand(1)));

    statements.push_back(std::make_unique<IRAdd>(IROperand(FP), IROperand(SP), IROperand(CALLEE_SAVES_SIZE * WORD_SIZE))); 
}

void append_ir_callee_restores(IRStatements& statements, int parameter_count)
{
    statements.push_back(std::make_unique<IRLoad>(IROperand(FP), IROperand(SP), IROperand(3)));
    statements.push_back(std::make_unique<IRLoad>(IROperand(SELF), IROperand(SP), IROperand(2)));
    statements.push_back(std::make_unique<IRLoad>(IROperand(RA), IROperand(SP), IROperand(1)));
    append_ir_stack_size_pop(statements, 3 + parameter_count);

    statements.push_back(std::make_unique<IRRegJump>(RA));
}

// IRAdd
void IRAdd::code(std::ostream& s) {
    if (rhs.kind == IROperand::Kind::IMM)
        // todo: I think we should use emit_addi here instead of emit_addiu
        emit_addiu(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.immValue, s);
    else
        emit_add(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRSub
void IRSub::code(std::ostream& s) {
    if (rhs.kind == IROperand::Kind::IMM)
        emit_addiu(dst.strValue.c_str(), lhs.strValue.c_str(), -rhs.immValue, s);
    else
        emit_sub(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRMul
void IRMul::code(std::ostream& s) {
    emit_mul(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRDiv
void IRDiv::code(std::ostream& s) {
    emit_div(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRNeg
void IRNeg::code(std::ostream& s) {
    emit_neg(dst.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRAssign
void IRAssign::code(std::ostream& s) {
    if (rhs.kind == IROperand::Kind::IMM)
        emit_load_imm(dst.strValue.c_str(), rhs.immValue, s);
    else
        emit_move(dst.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRMove
void IRMove::code(std::ostream& s) {
    emit_move(dst.strValue.c_str(), rhs.strValue.c_str(), s);
}

// IRLabel
void IRLabel::code(std::ostream& s) {
    s << label.strValue << ":" << std::endl;
}

// IRLabelJump
void IRLabelJump::code(std::ostream& s) {
    emit_jump(labelDst.strValue.c_str(), s);
}

// IRLabelJumpAndLink
void IRLabelJumpAndLink::code(std::ostream& s) {
    emit_jal(labelDst.strValue.c_str(), s);
}

// IRRegJump
void IRRegJump::code(std::ostream& s) {
    if (dst.strValue != RA) abort(); // IRRegJump should only be used for RA right now
    emit_return(s);
}

void IRRegJumpAndLink::code(std::ostream& s) {
    emit_jalr(dst.strValue.c_str(), s); // jalr is used for register jumps and links
}

void IRRelOp::code(std::ostream& s) {
    // Only handle IRRelOp::Kind::IR_LT, IR_LEQ, IR_EQ, IR_NEQ
    switch (kind) {
        case Kind::IR_LT:
            if (rhs.kind == IROperand::Kind::IMM)
                emit_slti(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.immValue, s);
            else
                emit_slt(dst.strValue.c_str(), lhs.strValue.c_str(), rhs.strValue.c_str(), s);
            break;
        case Kind::IR_LEQ:
        case Kind::IR_EQ:
        case Kind::IR_NEQ:
        default:
            abort(); // IRRelOp::Kind::IR_LEQ, IR_EQ, IR_NEQ are not handled here, currently not used in lowering from COOL to IR
            break;
    }
}

void IRIfJump::code(std::ostream& s) {
    // Only handle IRRelOp::Kind::IR_EQ, IR_LT, IR_LEQ, IR_NEQ
    switch (condition.kind) {
        case IRRelOp::Kind::IR_EQ:
            emit_beq(condition.lhs.strValue.c_str(), condition.rhs.strValue.c_str(), labelDst.strValue.c_str(), s);
            break;
        case IRRelOp::Kind::IR_NEQ:
            emit_bne(condition.lhs.strValue.c_str(), condition.rhs.strValue.c_str(), labelDst.strValue.c_str(), s);
            break;
        case IRRelOp::Kind::IR_LT:
            emit_blt(condition.lhs.strValue.c_str(), condition.rhs.strValue.c_str(), labelDst.strValue.c_str(), s);
            break;
        case IRRelOp::Kind::IR_LEQ:
            emit_bleq(condition.lhs.strValue.c_str(), condition.rhs.strValue.c_str(), labelDst.strValue.c_str(), s);
            break;
        default:
            // fallback: do nothing or abort
            break;
    }
}

void IRLoad::code(std::ostream& s) {
    if (get_src().kind == IROperand::Kind::LBL)
        emit_load_address(get_dst().strValue.c_str(), get_src().strValue.c_str(), s);
    else if (get_src().kind == IROperand::Kind::IMM)
        emit_load_imm(get_dst().strValue.c_str(), get_src().immValue, s);
    else
        emit_load(get_dst().strValue.c_str(), get_offset().immValue, get_src().strValue.c_str(), s);
}

void IRStore::code(std::ostream& s) {
    emit_store(get_src().strValue.c_str(), get_offset().immValue, get_dst().strValue.c_str(), s);
}