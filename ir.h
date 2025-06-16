#pragma once

#include <string>
#include "emit.h"

enum class IROpcode {
    // binary operations:
    IR_ADD, IR_SUB, IR_MUL, IR_DIV, 
    // relational operations:
    IR_LT, IR_LEQ, IR_EQ, 
    // unary operations:
    IR_NEG, IR_ASSIGN, 
    // labels
    IR_LABEL, 
    // control flow with conditions:
    IR_JUMP, IR_IF_JUMP, 
    // memory operations:
    IR_LOAD, IR_STORE, 
    // default/no-op:
    IR_NOP
};

// Note that IR Operands can only write to registers and can read from registers or immediate values.
struct IROperand {
    enum class Kind { UNINITIALIZED, REG, IMM } kind;
    std::string value;
    IROperand() : kind(Kind::UNINITIALIZED), value("") {}
    IROperand(Kind k, const std::string& val) : kind(k), value(val) {
        //todo: maybe want to add checks here to make sure that the value is a valid register or imm value
    }
};

class IRStatement {
    public:
    IROpcode opcode = IROpcode::IR_NOP;

    // code to mips asm
    // todo: make this pure virtual
    void code(ostream& s);

    protected:
    IRStatement(IROpcode op) : opcode(op) {}
};

class IRBinaryOp : public IRStatement {
    public:
    IROperand lhs;
    IROperand rhs;
    IROperand result;

    protected:
    IRBinaryOp(IROpcode op, const IROperand& l, const IROperand& r, const IROperand& res)
        : lhs(l), rhs(r), result(res), IRStatement(op) {}
};

class IRUnaryOp : public IRStatement {
    public:
    IROperand rhs;
    IROperand result;

    protected:
    IRUnaryOp(IROpcode op, const IROperand& opnd, const IROperand& res)
        : rhs(opnd), result(res), IRStatement(op) {}
};

class IRPlus : public IRBinaryOp {
    public:
    IRPlus(const IROperand& l, const IROperand& r, const IROperand& res)
        : IRBinaryOp(IROpcode::IR_ADD, l, r, res) {}
};

class IRSub : public IRBinaryOp {
    public:
    IRSub(const IROperand& l, const IROperand& r, const IROperand& res)
        : IRBinaryOp(IROpcode::IR_SUB, l, r, res) {}
};

class IRMul : public IRBinaryOp {
    public:
    IRMul(const IROperand& l, const IROperand& r, const IROperand& res)
        : IRBinaryOp(IROpcode::IR_MUL, l, r, res) {}
};

class IRDiv : public IRBinaryOp {
    public:
    IRDiv(const IROperand& l, const IROperand& r, const IROperand& res)
        : IRBinaryOp(IROpcode::IR_DIV, l, r, res) {}
};

class IRNeg : public IRUnaryOp {
    public:
    IRNeg(const IROperand& opnd, const IROperand& res)
        : IRUnaryOp(IROpcode::IR_NEG, opnd, res) {}
};

class IRAssign : public IRUnaryOp {
    public:
    IRAssign(const IROperand& opnd, const IROperand& res)
        : IRUnaryOp(IROpcode::IR_ASSIGN, opnd, res) {}
};

class IRLabel : public IRStatement {
    public:
    std::string label;

    IRLabel(const std::string& lbl) : label(lbl), IRStatement(IROpcode::IR_LABEL) {}
};

class IRJump : public IRStatement {
    public:
    std::string label;

    IRJump(const std::string& lbl) : label(lbl), IRStatement(IROpcode::IR_JUMP) {}
};

class IRIfJump : public IRStatement {
    public:
    IRStatement condition;
    std::string label;

    IRIfJump(const IRStatement& cond, const std::string& lbl)
        : condition(cond), label(lbl), IRStatement(IROpcode::IR_IF_JUMP) {}
};

class IRMemOP : public IRBinaryOp {
    public:
    int offset;
    
    IRMemOP(const IROperand& dst, const IROperand& src, const IROperand& res, int o)
        : offset(o), IRBinaryOp(IROpcode::IR_STORE, dst, src, res) {}
};

class IRLoad : public IRMemOP {
    // dst must be register that will store the value
    // src must be a register containing a memory address or immediate to load into dst
    // src can also be a label that will be converted into an address to load into dst
    // offset is an offset applied to the src register to determine the address to write to
};

class IRStore : public IRMemOP {
    // dst must be register that contains the memory address to write to
    // src must be a register that stores the 32 bits to write to dest
    // offset is an offset applied to the dst register to determine the address to write to
};
