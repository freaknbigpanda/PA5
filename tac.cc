#include "tac.h"
#include "emit.h" 
#include "cgen.h"
#include "cgen_gc.h"

void IRInstruction::code(std::ostream& s, CgenNode* cgen_node, SymbolTable<std::string, int> formals_table, int& sp, int num_params) const {
    switch (op) {
        case IROpcode::IR_ADD:
            emit_add(dst.value.c_str(), src1.value.c_str(), src2.value.c_str(), s);
            break;
        case IROpcode::IR_SUB:
            emit_sub(dst.value.c_str(), src1.value.c_str(), src2.value.c_str(), s);
            break;
        case IROpcode::IR_MUL:
            emit_mul(dst.value.c_str(), src1.value.c_str(), src2.value.c_str(), s);
            break;
        case IROpcode::IR_DIV:
            emit_div(dst.value.c_str(), src1.value.c_str(), src2.value.c_str(), s);
            break;
        case IROpcode::IR_CONST:
            // emit_load_imm(dst.name.c_str(), src1.value, s);
            // emit_load_int(ACC,inttable.lookup_string(src1.value),s);
            switch (src1.kind) {
                case IROperand::Kind::VAR:
                    emit_move(dst.value.c_str(), ACC, s);
                    break;
                case IROperand::Kind::CONST_INT:
                    emit_load_int(dst.value.c_str(), inttable.lookup_string(src1.value.c_str()), s);
                    break;
                case IROperand::Kind::CONST_BOOL:
                    if (src1.value == "true") {
                        emit_load_bool(dst.value.c_str(), BoolConst(1), s);
                    } else if (src1.value == "false") {
                        emit_load_bool(dst.value.c_str(), BoolConst(0), s);
                    } else {
                        abort(); // Invalid boolean constant
                    }
                    break;
                case IROperand::Kind::CONST_STR:
                    emit_load_string(dst.value.c_str(), stringtable.lookup_string(src1.value.c_str()), s);
                    break;
                default:
                    abort(); // Invalid operand kind for constant
            }
            break;
        case IROpcode::IR_RET:
            emit_move(ACC, dst.value.c_str(), s);
            emit_return(s);
            break;
        // case IROpcode::IR_NEG:
        //     emit_neg(dst.name.c_str(), src1.name.c_str(), s);
        //     break;
        case IROpcode::IR_ASSIGN:
            {
                int* fp_offset = formals_table.lookup(dst.value);
                int attribute_location = cgen_node->get_attribute_location(dst.value);
                if (fp_offset != nullptr)
                {
                    emit_store(ACC, *fp_offset, FP, s);
                }
                else if (attribute_location != -1)
                {
                    //assign to attribute
                    emit_store(ACC, 3 + attribute_location, SELF, s);
                    
                    if (cgen_Memmgr != GC_NOGC)
                    {
                        emit_addiu(A1, SELF, (3 + attribute_location) * WORD_SIZE, s);
                        emit_jal("_GenGC_Assign", sp, 0, s);
                    }
                }
                //todo: will probably need to handle other registers here as well
                else if (dst.value == ACC)
                {
                    emit_move(ACC, src1.value.c_str(), s); // If dst is ACC, just move src1 to ACC
                }
                else
                {
                    abort(); // Always be able to find 
                }

            }
            break;
        // case IROpcode::IR_LABEL:
        //     s << label << ":\n";
        //     break;
        // case IROpcode::IR_GOTO:
        //     emit_jump(label.c_str(), s);
        //     break;
        // case IROpcode::IR_IF_GOTO:
        //     emit_beqz(src1.name.c_str(), label, s);
        //     break;
        // case IROpcode::IR_PARAM:
        //     // Convention: push parameter onto stack
        //     emit_push(src1.name.c_str(), s);
        //     break;
        // case IROpcode::IR_CALL:
        //     emit_jal(src1.name.c_str(), num parameters, 0, s); // You may want to pass actual sp/num_params
        //     emit_move(dst.name.c_str(), ACC, s); // Move result from ACC to dst
        //     break;
        // case IROpcode::IR_LT:
        //     emit_slt(dst.name.c_str(), src1.name.c_str(), src2.name.c_str(), s);
        //     break;
        // case IROpcode::IR_LEQ:
        //     emit_bleq(src1.name.c_str(), src2.name.c_str(), /*label*/0, s); // You may want to handle this differently
        //     break;
        // case IROpcode::IR_EQ:
        //     emit_beq(src1.name.c_str(), src2.name.c_str(), /*label*/0, s); // You may want to handle this differently
        //     break;
        // case IROpcode::IR_COMP:
        //     emit_neg(dst.name.c_str(), src1.name.c_str(), s); // Placeholder for logical not
        //     break;
        // case IROpcode::IR_NOP:
        default:
            abort(); // want to catch unhandled cases
            break;
    }
}