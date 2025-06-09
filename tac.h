#pragma once
// Description: This header file defines the structures and enums for a simple three-address code (TAC) representation.
// It includes definitions for instructions, operands, and basic blocks used in the intermediate representation of a program.
// It is intended to be used in a compiler backend for code generation or optimization
#include <string>
#include <vector>

enum class IROpcode {
    IR_ADD, IR_SUB, IR_MUL, IR_DIV, IR_NEG,
    IR_LT, IR_LEQ, IR_EQ, IR_COMP,
    IR_ASSIGN, IR_LABEL, IR_GOTO, IR_IF_GOTO,
    IR_CONST, IR_PARAM, IR_CALL, IR_RET,
    IR_LOAD, IR_STORE, IR_NOP
};

struct IROperand {
    enum class Kind { VAR, CONST } kind;
    std::string name;
    int value;
    IROperand() : kind(Kind::VAR), value(0) {}
    IROperand(Kind k, const std::string& n) : kind(k), name(n), value(0) {}
    IROperand(Kind k, int v) : kind(k), value(v) {}
};

struct IRInstruction {
    IROpcode op;
    IROperand dst, src1, src2;
    std::string label;
    IRInstruction() : op(IROpcode::IR_NOP) {}
    IRInstruction(IROpcode o, IROperand d, IROperand s1, IROperand s2)
        : op(o), dst(d), src1(s1), src2(s2) {}
    IRInstruction(IROpcode o, IROperand d, IROperand s1)
        : op(o), dst(d), src1(s1) {}
    IRInstruction(IROpcode o, IROperand d)
        : op(o), dst(d) {}
    IRInstruction(IROpcode o, const std::string& l)
        : op(o), label(l) {}
};

class BasicBlock {
public:
    std::string label;
    std::vector<IRInstruction> instructions;
    std::vector<std::string> successors; // labels of successor blocks

    BasicBlock(const std::string &lbl) : label(lbl) {}

    void add_instruction(const IRInstruction &instr) {
        instructions.push_back(instr);
    }

    void add_successor(const std::string &succ) {
        successors.push_back(succ);
    }
};