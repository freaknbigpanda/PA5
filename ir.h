#pragma once

#include <string>
#include <vector>
#include <memory>
#include <limits>
#include <iostream>

// Note that IR Operands can only write to registers and can read from registers or immediate values.
struct IROperand {
    enum class Kind { UNINITIALIZED, REG, IMM, LBL } kind = Kind::UNINITIALIZED;
    std::string strValue = "";
    int64_t immValue = std::numeric_limits<int64_t>::min();
    IROperand(const std::string& val) : kind(Kind::REG), strValue(val) {};
    IROperand(int64_t val) : kind(Kind::IMM), immValue(val) {};

    virtual void print(std::ostream& os) const {
        if (immValue != std::numeric_limits<int64_t>::min()) {
            os << immValue;
        } else if (strValue != "") {
            os << strValue;
        } else {
            abort(); // operands should always have a value
        }
    }

    friend std::ostream& operator<< (std::ostream& os, const IROperand& op);
};

inline std::ostream& operator<< (std::ostream& os, const IROperand& op) {
    op.print(os);
    return os;
}

struct IRLabelOperand : IROperand {
    IRLabelOperand(const std::string& label) : IROperand(label) {
        kind = IROperand::Kind::LBL;
    }
};

class IRStatement {
    public:

    // code to mips asm
    virtual void code(std::ostream& s) = 0;
    friend std::ostream& operator<< (std::ostream& os, const IRStatement& stm);

    protected:
    bool should_indent = true;
    virtual void print(std::ostream& os) const {
        os << "no print function defined";
    }
};

inline std::ostream& operator<< (std::ostream& os, const IRStatement& stm) {
    if (stm.should_indent) {
        os << "    ";
    }
    stm.print(os); 
    return os;
}

class IRBinaryOp : public IRStatement {
    public:
    IROperand lhs;
    IROperand rhs;
    IROperand dst;
    std::string op_char = "";

    protected:
    IRBinaryOp(const IROperand& d, const IROperand& l, const IROperand& r)
        : lhs(l), rhs(r), dst(d) {}

    void print(std::ostream& os) const override {
        os << dst << " = " << lhs << " " << op_char << " " << rhs;
    }
};

class IRUnaryOp : public IRStatement {
    public:
    IROperand rhs;
    IROperand dst;
    std::string op_char = "";

    protected:
    IRUnaryOp(const IROperand& d, const IROperand& r)
        : rhs(r), dst(d) {}

    virtual void print(std::ostream& os) const override {
        os << dst << " = " << op_char << " " << rhs;
    }
};

class IRRelOp : public IRBinaryOp {
    public:
    enum class Kind { UNINITIALIZED, IR_LT, IR_LEQ, IR_EQ, IR_NEQ } kind = Kind::UNINITIALIZED;
    IRRelOp(const IROperand& l, const IROperand& r, IRRelOp::Kind k) 
        : IRBinaryOp(IROperand(std::numeric_limits<int64_t>::min()), l, r), kind(k) { set_op_char(); }
    IRRelOp(const IROperand& dst, const IROperand& l, const IROperand& r, IRRelOp::Kind k) 
        : IRBinaryOp(dst, l, r), kind(k) { set_op_char(); }  
    
    void code(std::ostream& s) override;

    protected:
    void print(std::ostream& os) const override {
        // this means that the dst register is unused 
        if (dst.immValue == std::numeric_limits<int64_t>::min()) {
            os << lhs << " " << op_char << " " << rhs;
        } else {
            IRBinaryOp::print(os);
        }
    }

    private:
    void set_op_char() {
        should_indent = false;
        switch (kind)
        {
        case Kind::UNINITIALIZED:
            abort();
            break;
        case Kind::IR_LT:
            op_char = "<";
            break;
        case Kind::IR_LEQ:
            op_char = "<=";
            break;
        case Kind::IR_EQ:
            op_char = "==";
            break;
        case Kind::IR_NEQ:
            op_char = "!=";
            break;

        default:
            break;
        }
    }
};

class IRMove : public IRUnaryOp {
public:
    IRMove(const IROperand& d, const IROperand& r)
        : IRUnaryOp(d, r) { }

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << dst << " = " << rhs;
    }
};

class IRAdd : public IRBinaryOp {
public:
    IRAdd(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) { op_char = "+"; }

    void code(std::ostream& s) override;
};

class IRSub : public IRBinaryOp {
public:
    IRSub(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) { op_char = "-"; }

    void code(std::ostream& s) override;
};

class IRMul : public IRBinaryOp {
public:
    IRMul(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) { op_char = "*"; }

    void code(std::ostream& s) override;
};

class IRDiv : public IRBinaryOp {
public:
    IRDiv(const IROperand& d, const IROperand& l, const IROperand& r)
        : IRBinaryOp(d, l, r) { op_char = "/"; }

    void code(std::ostream& s) override;
};

class IRNeg : public IRUnaryOp {
public:
    IRNeg(const IROperand& d, const IROperand& r)
        : IRUnaryOp(d, r) { op_char = "~"; }

    void code(std::ostream& s) override;
};

class IRAssign : public IRUnaryOp {
public:
    IRAssign(const IROperand& d, const IROperand& r)
        : IRUnaryOp(d, r) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << dst << " = " << rhs;
    }
};

class IRLabel : public IRStatement {
public:
    IRLabelOperand label;

    IRLabel(const std::string& lbl) : label(lbl) { should_indent = false; }

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << label << ":";
    }
};

class IRRegJump : public IRStatement {
public:
    IROperand dst;

    IRRegJump(const std::string& reg) : dst(reg) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "JUMP " << dst;
    }
};

class IRRegJumpAndLink : public IRStatement {
public:
    IROperand dst;

    IRRegJumpAndLink(const std::string& reg) : dst(reg) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "JUMP AND LINK " << dst;
    }
};

class IRLabelJump : public IRStatement {
public:
    IRLabelOperand labelDst;

    IRLabelJump(const std::string& label) : labelDst(IRLabelOperand(label)) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "JUMP " << labelDst;
    }
};

class IRLabelJumpAndLink : public IRStatement {
public:
    IRLabelOperand labelDst;

    IRLabelJumpAndLink(const std::string& label) : labelDst(IRLabelOperand(label)) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "JUMP AND LINK " << labelDst;
    }
};

class IRIfJump : public IRStatement {
public:
    IRRelOp condition;
    IRLabelOperand labelDst;

    IRIfJump(const IRRelOp& cond, const std::string& lbl)
        : condition(cond), labelDst(IRLabelOperand(lbl)) {}

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "IF " << condition << " JUMP " << labelDst;
    }
};

class IRMemOP : public IRBinaryOp {
    public:
    enum class Kind { UNINITIALIZED, LOAD, STORE } kind = Kind::UNINITIALIZED;
    
    IRMemOP(const IROperand& d, const IROperand& s, const IROperand& offset)
        : IRBinaryOp(d, s, offset) {}

    IROperand get_dst() const { return dst; }
    IROperand get_src() const { return lhs; }
    IROperand get_offset() const { return rhs; }
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

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << dst << " <- (" << rhs << ")" << lhs;
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

    void code(std::ostream& s) override;

protected:
    virtual void print(std::ostream& os) const override {
        os << "(" <<  rhs << ")" << dst << " <- " << lhs;
    }
};

using IRStatements = std::vector<std::unique_ptr<IRStatement>>;
void append_ir_callee_saves(IRStatements& statments);
void append_ir_callee_restores(IRStatements& statments, int parameter_count);
void append_ir_stack_size_push(IRStatements& statements, int num_words);
void append_ir_stack_size_pop(IRStatements& statements, int num_words);
void append_ir_object_copy(IRStatements& statements);