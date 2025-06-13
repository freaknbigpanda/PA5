#pragma once
// Description: This header file defines the structures and enums for a simple three-address code (TAC) representation.
// It includes definitions for instructions, operands, and basic blocks used in the intermediate representation of a program.
// It is intended to be used in a compiler backend for code generation or optimization
#include <string>
#include <vector>
#include <iostream>
#include <ostream>
#include "symtab.h"

enum class IROpcode {
    IR_ADD, IR_SUB, IR_MUL, IR_DIV, IR_NEG,
    IR_LT, IR_LEQ, IR_EQ, IR_COMP,
    IR_ASSIGN, IR_LABEL, IR_GOTO, IR_IF_GOTO,
    IR_CONST, IR_PARAM, IR_CALL, IR_RET,
    IR_LOAD, IR_STORE, IR_NOP
};

struct IROperand {
    enum class Kind { UNINITIALIZED, VAR, CONST_INT, CONST_BOOL, CONST_STR } kind;
    std::string value;
    IROperand() : kind(Kind::UNINITIALIZED), value("") {}
    IROperand(Kind k, const std::string& val) : kind(k), value(val) {}
};

class CgenNode; // Forward declaration for CgenNode

class IRInstruction {
public:
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

    // Generate MIPS assembly for this IR instruction
    void code(std::ostream& s, CgenNode* cgen_node, SymbolTable<std::string, int> formals_table, int& sp, int num_params) const;
};

// Helper for printing IROpcode as string
inline std::string to_string(IROpcode op) {
    switch (op) {
        case IROpcode::IR_ADD: return "ADD";
        case IROpcode::IR_SUB: return "SUB";
        case IROpcode::IR_MUL: return "MUL";
        case IROpcode::IR_DIV: return "DIV";
        case IROpcode::IR_NEG: return "NEG";
        case IROpcode::IR_LT: return "LT";
        case IROpcode::IR_LEQ: return "LEQ";
        case IROpcode::IR_EQ: return "EQ";
        case IROpcode::IR_COMP: return "COMP";
        case IROpcode::IR_ASSIGN: return "ASSIGN";
        case IROpcode::IR_LABEL: return "LABEL";
        case IROpcode::IR_GOTO: return "GOTO";
        case IROpcode::IR_IF_GOTO: return "IF_GOTO";
        case IROpcode::IR_CONST: return "CONST";
        case IROpcode::IR_PARAM: return "PARAM";
        case IROpcode::IR_CALL: return "CALL";
        case IROpcode::IR_RET: return "RET";
        case IROpcode::IR_LOAD: return "LOAD";
        case IROpcode::IR_STORE: return "STORE";
        case IROpcode::IR_NOP: return "NOP";
        default: return "UNKNOWN";
    }
}

// Helper for printing IROperand
inline std::ostream& operator<<(std::ostream& os, const IROperand& op) {
    switch (op.kind) {
        case IROperand::Kind::VAR:        os << op.value; break;
        case IROperand::Kind::CONST_INT:  os << "#" << op.value; break;
        case IROperand::Kind::CONST_BOOL: os << "BOOLEAN " << op.value; break;
        case IROperand::Kind::CONST_STR:  os << "\"" << op.value << "\""; break;
        default:                          os << "_"; break;
    }
    return os;
}

// Stream operator for IRInstruction
inline std::ostream& operator<<(std::ostream& os, const IRInstruction& instr) {
    os << to_string(instr.op);
    // Print operands if present
    if (instr.op == IROpcode::IR_LABEL) {
        os << " " << instr.label;
    } else if (instr.op == IROpcode::IR_GOTO || instr.op == IROpcode::IR_IF_GOTO) {
        os << " " << instr.label;
        if (instr.src1.kind != IROperand::Kind::UNINITIALIZED)
            os << ", " << instr.src1;
    } else if (instr.op == IROpcode::IR_CONST) {
        os << " " << instr.dst << ", " << instr.src1;
    } else if (instr.op == IROpcode::IR_ASSIGN || instr.op == IROpcode::IR_NEG || instr.op == IROpcode::IR_RET || instr.op == IROpcode::IR_PARAM || instr.op == IROpcode::IR_CALL || instr.op == IROpcode::IR_LOAD || instr.op == IROpcode::IR_STORE) {
        os << " " << instr.dst << ", " << instr.src1;
    } else if (instr.op != IROpcode::IR_NOP) {
        os << " " << instr.dst << ", " << instr.src1 << ", " << instr.src2;
    }
    return os;
}

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
