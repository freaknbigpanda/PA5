#pragma once

#include <string>
#include <vector>
#include <limits>
#include "emit.h"

// todo: pretty sure these opcodes are useless because we encode this information in the class hierarchy
// todo: clean up the call sites for all of these IRStatements, we shouldn't ever have to instantiate IROperand at callsite
// enum class IROpcode {
//     // binary operations:
//     IR_ADD, IR_SUB, IR_MUL, IR_DIV, 
//     // relational operations:
//     IR_LT, IR_LEQ, IR_EQ, 
//     // unary operations:
//     IR_NEG, IR_ASSIGN, 
//     // labels
//     IR_LABEL, 
//     // control flow with conditions:
//     IR_JUMP, IR_IF_JUMP, 
//     // memory operations:
//     IR_LOAD, IR_STORE, 
//     // default/no-op:
//     IR_NOP
// };

// Note that IR Operands can only write to registers and can read from registers or immediate values.
struct IROperand {
    enum class Kind { UNINITIALIZED, REG, IMM, LBL } kind = Kind::UNINITIALIZED;
    std::string strValue = "";
    int64_t immValue = std::numeric_limits<int64_t>::min();
    IROperand(const std::string& val) : kind(Kind::REG), strValue(val) {};
    IROperand(int64_t val) : kind(Kind::IMM), immValue(val) {};
};

struct IRLabelOperand : IROperand {
    IRLabelOperand(const std::string& label) : IROperand(label) {
        kind = IROperand::Kind::LBL;
    }
};

class IRStatement {
    public:

    // code to mips asm
    // todo: make this pure virtual
    void code(ostream& s);
};

class IRBinaryOp : public IRStatement {
    public:
    IROperand lhs;
    IROperand rhs;
    IROperand dst;

    protected:
    IRBinaryOp(const IROperand& d, const IROperand& l, const IROperand& r)
        : lhs(l), rhs(r), dst(d) {}
};

class IRUnaryOp : public IRStatement {
    public:
    IROperand rhs;
    IROperand dst;

    protected:
    IRUnaryOp(const IROperand& d, const IROperand& r)
        : rhs(r), dst(d) {}
};

class IRPlus : public IRBinaryOp {
    public:
    IRPlus(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) {}
};

class IRSub : public IRBinaryOp {
    public:
    IRSub(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) {}
};

class IRMul : public IRBinaryOp {
    public:
    IRMul(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) {}
};

class IRDiv : public IRBinaryOp {
    public:
    IRDiv(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) {}
};

class IRNeg : public IRUnaryOp {
    public:
    IRNeg(const IROperand& d, const IROperand& r)
        : IRUnaryOp(d, r) {}
};

class IRAssign : public IRUnaryOp {
    public:
    IRAssign(const IROperand& d, const IROperand& r)
        : IRUnaryOp(d, r) {}
};

class IRLabel : public IRStatement {
    public:
    IRLabelOperand label;

    IRLabel(const std::string& lbl) : label(lbl) {}
};

class IRRegJump : public IRStatement {
    public:
    IROperand dst;

    IRRegJump(const std::string& reg) : dst(reg) {}
};

class IRLableJump : public IRStatement {
    public:
    IRLabelOperand labelDst;

    IRLableJump(const std::string& label) : labelDst(IRLabelOperand(label)) {}
};

class IRLabelJumpAndLink : public IRStatement {
    IRLabelOperand labelDst;

    IRLabelJumpAndLink(const std::string& label) : labelDst(IRLabelOperand(label)) {}
};

class IRIfJump : public IRStatement {
    public:
    IRStatement condition;
    std::string label;

    IRIfJump(const IRStatement& cond, const std::string& lbl)
        : condition(cond), label(lbl) {}
};

class IRMemOP : public IRBinaryOp {
    public:
    enum class Kind { UNINITIALIZED, LOAD, STORE } kind = Kind::UNINITIALIZED;
    
    IRMemOP(const IROperand& d, const IROperand& s, const IROperand& offset)
        : IRBinaryOp(d, s, offset) {}
};

class IRLoad : public IRMemOP {
    public:
    // dst must be register that will store the value
    // src must be a register containing a memory address or immediate to load into dst
    // src can also be a label that will be converted into an address to load into dst
    // offset is an offset applied to the src register to determine the address to write to

    IRLoad(const IROperand& d, const IROperand& s, const IROperand& offset) 
    : IRMemOP(d, s, offset) {
        kind = IRMemOP::Kind::LOAD;
    }
};

class IRStore : public IRMemOP {
    public:
    // dst must be register that contains the memory address to write to
    // src must be a register that stores the 32 bits to write to dest
    // offset is an offset applied to the dst register to determine the address to write to
    IRStore(const IROperand& d, const IROperand& s, const IROperand& offset) 
    : IRMemOP(d, s, offset) {
        kind = IRMemOP::Kind::STORE;
    }
};

void append_ir_method_prefix(std::vector<IRStatement>& statments, int parameter_count, int& sp);
void append_ir_method_suffix(std::vector<IRStatement>& statments, int parameter_count);
