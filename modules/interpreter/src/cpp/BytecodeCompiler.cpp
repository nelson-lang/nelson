//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include "BytecodeCompiler.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "Keywords.hpp"
#include "LexerContext.hpp"
#include "SlotMap.hpp"
#include "TextToNumber.hpp"
#include "Decomplexify.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    [[noreturn]] void
    unsupported(AbstractSyntaxTreePtr node, const std::string& what)
    {
        std::string detail = what;
        if (node != nullptr) {
            detail += " (node type=" + std::to_string(static_cast<int>(node->type))
                + ", op=" + std::to_string(static_cast<int>(node->opNum)) + ")";
        }
        Error(_("Bytecode compiler unsupported construct: ") + detail);
    }
    //=============================================================================
    SourceSpan
    spanFromContext(int context)
    {
        SourceSpan span;
        span.line = static_cast<uint32_t>(context & 0x0000FFFF);
        span.col = static_cast<uint16_t>((context >> 16) & 0xFFFF);
        if (span.line == 0) {
            span.line = 1;
        }
        return span;
    }
    //=============================================================================
    std::string
    classNameLeaf(const std::string& className)
    {
        size_t pos = className.find_last_of('.');
        if (pos == std::string::npos || pos + 1 >= className.size()) {
            return className;
        }
        return className.substr(pos + 1);
    }
    //=============================================================================
    bool
    looksLikeRuntimeClassdefReference(const std::string& className)
    {
        std::string leaf = classNameLeaf(className);
        return !leaf.empty() && std::isupper(static_cast<unsigned char>(leaf.front())) != 0;
    }
    //=============================================================================
    OpCode
    binaryOpcode(OP_TYPE op)
    {
        switch (op) {
        case OP_PLUS:
            return OpCode::OP_PLUS;
        case OP_SUBTRACT:
            return OpCode::OP_MINUS;
        case OP_TIMES:
            return OpCode::OP_MTIMES;
        case OP_DOT_TIMES:
            return OpCode::OP_TIMES;
        case OP_RDIV:
            return OpCode::OP_MRDIV;
        case OP_DOT_RDIV:
            return OpCode::OP_RDIV;
        case OP_LDIV:
            return OpCode::OP_MLDIV;
        case OP_DOT_LDIV:
            return OpCode::OP_LDIV;
        case OP_MPOWER:
            return OpCode::OP_MPOWER;
        case OP_POWER:
            return OpCode::OP_POWER;
        case OP_LT:
            return OpCode::OP_LT;
        case OP_LEQ:
            return OpCode::OP_LEQ;
        case OP_GT:
            return OpCode::OP_GT;
        case OP_GEQ:
            return OpCode::OP_GEQ;
        case OP_EQ:
            return OpCode::OP_EQ;
        case OP_NEQ:
            return OpCode::OP_NEQ;
        case OP_AND:
            return OpCode::OP_AND;
        case OP_OR:
            return OpCode::OP_OR;
        default:
            throw std::invalid_argument("not a supported binary opcode");
        }
    }
    //=============================================================================
    OpCode
    unaryOpcode(OP_TYPE op)
    {
        switch (op) {
        case OP_UPLUS:
            return OpCode::OP_UPLUS;
        case OP_UMINUS:
            return OpCode::OP_UMINUS;
        case OP_NOT:
            return OpCode::OP_NOT;
        case OP_TRANSPOSE:
            return OpCode::OP_TRANSPOSE;
        case OP_DOT_TRANSPOSE:
            return OpCode::OP_DOT_TRANSPOSE;
        default:
            throw std::invalid_argument("not a supported unary opcode");
        }
    }
    //=============================================================================
    std::string
    joinStrings(const stringVector& values)
    {
        std::string result;
        for (size_t k = 0; k < values.size(); ++k) {
            if (k != 0) {
                result.push_back('\x1f');
            }
            result += values[k];
        }
        return result;
    }
    //=============================================================================
    class CompilerContext
    {
    public:
        CompilerContext(BytecodeChunk& chunk, SlotMap& slots) : chunk_(chunk), slots_(slots) { }

        void
        compileBlock(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr) {
                return;
            }
            if (node->opNum != OP_BLOCK) {
                unsupported(node, "block root");
            }
            AbstractSyntaxTreePtr statement = node->down;
            while (statement != nullptr) {
                compileStatement(statement);
                statement = statement->right;
            }
            chunk_.nLocals = slots_.totalSlots();
            if (chunk_.maxStack < maxStack_) {
                chunk_.maxStack = maxStack_;
            }
        }

    private:
        struct LoopPatchContext
        {
            std::vector<uint32_t> breaks;
            std::vector<uint32_t> continues;
        };

        BytecodeChunk& chunk_;
        SlotMap& slots_;
        uint16_t stackDepth_ = 0;
        uint16_t maxStack_ = 0;
        uint16_t nextLoopSlot_ = 0;
        uint16_t statementCounter_ = 0;
        std::vector<LoopPatchContext> loopStack_;
        std::vector<uint16_t> activeLoopLocalSlots_;

        uint32_t
        emit(OpCode op, uint8_t flags = 0, uint16_t A = 0, uint16_t B = 0, uint16_t C = 0)
        {
            return chunk_.emit(op, flags, A, B, C);
        }

        uint16_t
        nameIndex(const std::string& name)
        {
            auto found = std::find(chunk_.names.begin(), chunk_.names.end(), name);
            if (found != chunk_.names.end()) {
                return static_cast<uint16_t>(std::distance(chunk_.names.begin(), found));
            }
            chunk_.names.push_back(name);
            return static_cast<uint16_t>(chunk_.names.size() - 1);
        }

        void
        push()
        {
            ++stackDepth_;
            if (stackDepth_ > maxStack_) {
                maxStack_ = stackDepth_;
            }
        }

        void
        pop(uint16_t count = 1)
        {
            if (stackDepth_ >= count) {
                stackDepth_ = static_cast<uint16_t>(stackDepth_ - count);
            } else {
                stackDepth_ = 0;
            }
        }

        void
        replaceBinary()
        {
            pop(2);
            push();
        }

        void
        patch(uint32_t instr, uint32_t target)
        {
            chunk_.patchJump(instr, static_cast<uint16_t>(target));
        }

        uint16_t
        allocLoopSlot()
        {
            return nextLoopSlot_++;
        }

        static uint16_t
        countPeers(AbstractSyntaxTreePtr node)
        {
            uint16_t count = 0;
            while (node != nullptr) {
                ++count;
                node = node->right;
            }
            return count;
        }

        static bool
        containsEndKeyword(AbstractSyntaxTreePtr node)
        {
            while (node != nullptr) {
                if (node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_END) {
                    return true;
                }
                if (containsEndKeyword(node->down)) {
                    return true;
                }
                node = node->right;
            }
            return false;
        }

        static bool
        containsDirectEndKeyword(AbstractSyntaxTreePtr node)
        {
            while (node != nullptr) {
                if (node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_END) {
                    return true;
                }
                node = node->right;
            }
            return false;
        }

        static bool
        allowsDirectEndAsFunctionArgument(const std::string& name)
        {
            return name == "min" || name == "max";
        }

        static bool
        isOneToEndExpr(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->type != non_terminal || node->opNum != OP_COLON
                || node->down == nullptr || node->down->right == nullptr
                || node->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr first = node->down;
            AbstractSyntaxTreePtr last = first->right;
            return (first->type == const_int_node || first->type == const_double_node)
                && first->text == "1" && last->type == reserved_node
                && last->tokenNumber == NLS_KEYWORD_END;
        }

        void
        emitDebugStatement(AbstractSyntaxTreePtr statement)
        {
            AbstractSyntaxTreePtr sourceNode
                = statement->down != nullptr ? statement->down : statement;
            SourceSpan span = spanFromContext(sourceNode->getContext());
            uint16_t spanIndex = static_cast<uint16_t>(chunk_.spans.size());
            uint32_t instrIndex = emit(OpCode::DEBUG_STMT, 0, spanIndex);
            chunk_.recordSpan(instrIndex, span.line, span.col, span.endLine);
        }

        void
        compileStatement(AbstractSyntaxTreePtr statement)
        {
            if (statement == nullptr || statement->isEmpty()) {
                return;
            }
            bool printIt = false;
            switch (statement->opNum) {
            case OP_QSTATEMENT:
                printIt = false;
                break;
            case OP_RSTATEMENT:
                printIt = true;
                break;
            default:
                unsupported(statement, "statement wrapper");
            }
            emitDebugStatement(statement);
            if ((++statementCounter_ & 63) == 0) {
                emit(OpCode::CHECK_INTERRUPT);
            }
            compileStatementType(statement->down, printIt);
        }

        void
        compileStatementType(AbstractSyntaxTreePtr node, bool printIt)
        {
            if (node == nullptr || node->isEmpty()) {
                emit(OpCode::NOP);
                return;
            }
            if (node->opNum == OP_ASSIGN) {
                compileAssign(node, printIt);
                return;
            }
            if (node->opNum == OP_MULTICALL) {
                compileMultiCall(node->down, printIt);
                return;
            }
            if (node->opNum == OP_SCALL) {
                compileSpecialCall(node->down, printIt);
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down == nullptr
                && node->down->text == "clear") {
                emit(OpCode::CALL_NAMED, 0, nameIndex(node->down->text), 0, 0);
                uint16_t totalSlots = slots_.totalSlots();
                for (uint16_t slot = 0; slot < totalSlots; ++slot) {
                    emit(OpCode::DELETE_LOCAL, 0, slot);
                }
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down == nullptr
                && printIt) {
                if (isDefinitelyAssignedLocal(node->down->text)) {
                    compileVarLoad(node->down);
                    emit(OpCode::DISPLAY_STACK, 0, nameIndex(node->down->text));
                    emit(OpCode::POP);
                    pop();
                } else {
                    emit(OpCode::CALL_NAMED, INST_FLAG_PRINT, nameIndex(node->down->text), 0, 0);
                }
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down == nullptr
                && !printIt) {
                emit(OpCode::CALL_NAMED, 0, nameIndex(node->down->text), 0, 0);
                if (node->down->text == "clear") {
                    uint16_t totalSlots = slots_.totalSlots();
                    for (uint16_t slot = 0; slot < totalSlots; ++slot) {
                        emit(OpCode::DELETE_LOCAL, 0, slot);
                    }
                }
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down != nullptr
                && isDefinitelyAssignedLocal(node->down->text) && !printIt) {
                compileNamedCallOrIndex(node->down, 0);
                emit(OpCode::POP);
                pop();
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down != nullptr
                && isDefinitelyAssignedLocal(node->down->text) && printIt) {
                compileNamedCallOrIndex(node->down, 1, true);
                emit(OpCode::DUP);
                push();
                emit(OpCode::STORE_ANS);
                pop();
                emit(OpCode::DISPLAY_ANS);
                emit(OpCode::POP);
                pop();
                return;
            }
            if (node->opNum == OP_RHS && node->down != nullptr && node->down->down != nullptr
                && !isDefinitelyAssignedLocal(node->down->text)) {
                if (printIt && shouldDisplayIndexedRhs(node->down)) {
                    compileNamedCallOrIndex(node->down, 1, true);
                    emit(OpCode::DUP);
                    push();
                    emit(OpCode::STORE_ANS);
                    pop();
                    emit(OpCode::DISPLAY_ANS);
                    emit(OpCode::POP);
                    pop();
                    return;
                }
                compileNamedCallOrIndex(node->down, 0, printIt);
                return;
            }
            if (node->type == reserved_node) {
                switch (node->tokenNumber) {
                case NLS_KEYWORD_FOR:
                    compileFor(node->down);
                    return;
                case NLS_KEYWORD_WHILE:
                    compileWhile(node->down);
                    return;
                case NLS_KEYWORD_IF:
                    compileIf(node->down);
                    return;
                case NLS_KEYWORD_TRY:
                    compileTry(node->down);
                    return;
                case NLS_KEYWORD_SWITCH:
                    compileSwitch(node->down);
                    return;
                case NLS_KEYWORD_BREAK:
                    compileBreak();
                    return;
                case NLS_KEYWORD_CONTINUE:
                    compileContinue();
                    return;
                case NLS_KEYWORD_RETURN:
                    emit(OpCode::RETURN);
                    return;
                case NLS_KEYWORD_ABORT:
                    emit(OpCode::ABORT);
                    return;
                default:
                    unsupported(node, "reserved statement");
                }
            }
            compileExpr(node);
            if (printIt) {
                emit(OpCode::DUP);
                push();
                emit(OpCode::STORE_ANS);
                pop();
                emit(OpCode::DISPLAY_ANS);
                emit(OpCode::POP);
                pop();
            } else {
                emit(OpCode::POP);
                pop();
            }
        }

        void
        compileBreak()
        {
            if (loopStack_.empty()) {
                emit(OpCode::NOP);
                return;
            }
            uint32_t instr = emit(OpCode::BREAK);
            loopStack_.back().breaks.push_back(instr);
        }

        void
        compileContinue()
        {
            if (loopStack_.empty()) {
                emit(OpCode::NOP);
                return;
            }
            uint32_t instr = emit(OpCode::CONTINUE);
            loopStack_.back().continues.push_back(instr);
        }

        void
        compileIf(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->opNum != OP_CSTAT || node->down == nullptr) {
                unsupported(node, "if statement");
            }

            std::vector<uint32_t> exitJumps;
            auto compileConditionedBlock = [&](AbstractSyntaxTreePtr conditioned) {
                AbstractSyntaxTreePtr condition = conditioned->down;
                AbstractSyntaxTreePtr codeBlock = condition ? condition->right : nullptr;
                compileExpr(condition);
                uint32_t falseJump = emit(OpCode::JUMP_IF_FALSE);
                pop();
                compileBlock(codeBlock);
                exitJumps.push_back(emit(OpCode::JUMP));
                patch(falseJump, static_cast<uint32_t>(chunk_.code.size()));
            };

            compileConditionedBlock(node);

            AbstractSyntaxTreePtr elseBlock = nullptr;
            AbstractSyntaxTreePtr elseOrElseIf = node->right;
            if (elseOrElseIf != nullptr && elseOrElseIf->opNum == OP_ELSEIFBLOCK) {
                AbstractSyntaxTreePtr current = elseOrElseIf->down;
                while (current != nullptr) {
                    compileConditionedBlock(current);
                    current = current->right;
                }
                elseBlock = elseOrElseIf->right;
            } else {
                elseBlock = elseOrElseIf;
            }

            if (elseBlock != nullptr && elseBlock->opNum == OP_BLOCK) {
                compileBlock(elseBlock);
            }
            uint32_t endPC = static_cast<uint32_t>(chunk_.code.size());
            for (uint32_t jump : exitJumps) {
                patch(jump, endPC);
            }
        }

        void
        compileWhile(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->right == nullptr) {
                return;
            }
            uint32_t headerPC = static_cast<uint32_t>(chunk_.code.size());
            compileExpr(node);
            uint32_t exitJump = emit(OpCode::WHILE_CHECK);
            pop();

            loopStack_.push_back(LoopPatchContext());
            compileBlock(node->right);
            uint32_t continuePC = headerPC;
            for (uint32_t continueJump : loopStack_.back().continues) {
                patch(continueJump, continuePC);
            }
            emit(OpCode::JUMP, 0, static_cast<uint16_t>(headerPC));
            uint32_t exitPC = static_cast<uint32_t>(chunk_.code.size());
            patch(exitJump, exitPC);
            for (uint32_t breakJump : loopStack_.back().breaks) {
                patch(breakJump, exitPC);
            }
            loopStack_.pop_back();
        }

        void
        compileFor(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->type != id_node || node->down == nullptr) {
                return;
            }
            auto slot = slots_.resolve(node->text);
            bool isGlobalLoopVariable = !slot.has_value() || slot->kind == SlotKind::Global;
            uint16_t variableOperand = isGlobalLoopVariable ? nameIndex(node->text) : slot->index;
            uint8_t loopFlags = isGlobalLoopVariable ? INST_FLAG_GLOBAL : 0;
            uint16_t loopSlot = allocLoopSlot();
            bool isRange = compileForRangeOperands(node->down);
            bool isDoubleCandidate = !isRange;
            uint32_t init = 0;
            if (isRange) {
                init = emit(OpCode::FOR_INIT_RANGE,
                    static_cast<uint8_t>(
                        loopFlags | (isTernaryColon(node->down) ? INST_FLAG_RANGE_HAS_STEP : 0)),
                    loopSlot, variableOperand);
                pop(isTernaryColon(node->down) ? 3 : 2);
            } else {
                compileExpr(node->down);
                init = emit(OpCode::FOR_INIT_DBL, loopFlags, loopSlot, variableOperand);
                pop();
            }
            if (!isGlobalLoopVariable) {
                slots_.markAssigned(node->text);
                activeLoopLocalSlots_.push_back(slot->index);
            }

            uint32_t bodyPC = static_cast<uint32_t>(chunk_.code.size());
            loopStack_.push_back(LoopPatchContext());
            compileBlock(node->right);
            uint32_t stepPC = static_cast<uint32_t>(chunk_.code.size());
            for (uint32_t continueJump : loopStack_.back().continues) {
                patch(continueJump, stepPC);
            }
            OpCode stepOp = isRange ? OpCode::FOR_STEP_RANGE
                                    : (isDoubleCandidate ? OpCode::FOR_STEP_DBL : OpCode::FOR_STEP);
            emit(stepOp, loopFlags, loopSlot, variableOperand, static_cast<uint16_t>(bodyPC));
            uint32_t exitPC = static_cast<uint32_t>(chunk_.code.size());
            chunk_.code[init].C = static_cast<uint16_t>(exitPC);
            for (uint32_t breakJump : loopStack_.back().breaks) {
                patch(breakJump, exitPC);
            }
            loopStack_.pop_back();
            if (!isGlobalLoopVariable) {
                activeLoopLocalSlots_.pop_back();
            }
            chunk_.nLoopSlots = std::max(chunk_.nLoopSlots, nextLoopSlot_);
        }

        void
        compileTry(AbstractSyntaxTreePtr tryBlock)
        {
            uint32_t tryBegin = emit(OpCode::TRY_BEGIN);
            compileBlock(tryBlock);
            emit(OpCode::TRY_END);
            uint32_t jumpEnd = emit(OpCode::JUMP);
            uint32_t catchPC = static_cast<uint32_t>(chunk_.code.size());
            patch(tryBegin, catchPC);

            AbstractSyntaxTreePtr catchNode = tryBlock != nullptr ? tryBlock->right : nullptr;
            if (catchNode != nullptr) {
                if (catchNode->type == id_node) {
                    auto slot = slots_.resolve(catchNode->text);
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::CATCH_BEGIN, INST_FLAG_GLOBAL, nameIndex(catchNode->text));
                    } else {
                        emit(OpCode::CATCH_BEGIN, 0, slot->index);
                        slots_.markAssigned(catchNode->text);
                    }
                    compileBlock(catchNode->down);
                } else {
                    compileBlock(catchNode);
                }
            }

            patch(jumpEnd, static_cast<uint32_t>(chunk_.code.size()));
        }

        void
        compileSwitch(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr) {
                unsupported(node, "switch statement");
            }
            compileExpr(node);
            AbstractSyntaxTreePtr caseBlock = node->right;
            AbstractSyntaxTreePtr otherwiseBlock = nullptr;
            std::vector<uint32_t> exitJumps;

            if (caseBlock != nullptr && caseBlock->opNum == OP_CASEBLOCK) {
                otherwiseBlock = caseBlock->right;
                AbstractSyntaxTreePtr caseNode = caseBlock->down;
                while (caseNode != nullptr) {
                    if (caseNode->type != reserved_node || caseNode->tokenNumber != NLS_KEYWORD_CASE
                        || caseNode->down == nullptr) {
                        unsupported(caseNode, "case statement");
                    }
                    emit(OpCode::DUP);
                    push();
                    compileExpr(caseNode->down);
                    emit(OpCode::CASE_MATCH);
                    pop(2);
                    push();
                    uint32_t nextCase = emit(OpCode::JUMP_IF_FALSE);
                    pop();
                    emit(OpCode::POP);
                    pop();
                    compileBlock(caseNode->down->right);
                    exitJumps.push_back(emit(OpCode::JUMP));
                    patch(nextCase, static_cast<uint32_t>(chunk_.code.size()));
                    stackDepth_ = 1;
                    caseNode = caseNode->right;
                }
            } else {
                otherwiseBlock = caseBlock;
            }

            emit(OpCode::POP);
            pop();
            if (otherwiseBlock != nullptr) {
                compileBlock(otherwiseBlock);
            }
            uint32_t endPC = static_cast<uint32_t>(chunk_.code.size());
            for (uint32_t jump : exitJumps) {
                patch(jump, endPC);
            }
        }

        static bool
        isTernaryColon(AbstractSyntaxTreePtr node)
        {
            return node != nullptr && node->opNum == OP_COLON && node->down != nullptr
                && node->down->opNum == OP_COLON;
        }

        bool
        compileForRangeOperands(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->opNum != OP_COLON || node->down == nullptr) {
                return false;
            }
            if (isTernaryColon(node)) {
                AbstractSyntaxTreePtr inner = node->down;
                compileExpr(inner->down);
                compileExpr(inner->down->right);
                compileExpr(inner->right);
                return true;
            }
            if (node->down->right == nullptr) {
                return false;
            }
            compileExpr(node->down);
            compileExpr(node->down->right);
            return true;
        }

        void
        compileAssign(AbstractSyntaxTreePtr node, bool printIt)
        {
            AbstractSyntaxTreePtr lhs = node->down;
            if (lhs == nullptr || lhs->type != id_node) {
                unsupported(node, "assignment lhs");
            }
            if (lhs->down != nullptr) {
                compileIndexedAssign(lhs, printIt);
                return;
            }
            AbstractSyntaxTreePtr rhs = lhs->right;
            if (rhs == nullptr) {
                unsupported(node, "assignment rhs");
            }
            compileExpr(rhs);
            auto slot = slots_.resolve(lhs->text);
            if (!slot.has_value() || slot->kind == SlotKind::Global) {
                emit(OpCode::STORE_GLOBAL, 0, nameIndex(lhs->text));
            } else {
                emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
            }
            pop();
        }

        void
        compileIndexedAssign(AbstractSyntaxTreePtr lhs, bool printIt)
        {
            if (lhs == nullptr || lhs->type != id_node || lhs->down == nullptr) {
                unsupported(lhs, "indexed assignment lhs");
            }
            AbstractSyntaxTreePtr rhs = lhs->right;
            if (rhs == nullptr) {
                unsupported(lhs, "indexed assignment rhs");
            }
            auto slot = slots_.resolve(lhs->text);
            bool useContextBase = !slot.has_value() || slot->kind == SlotKind::Global
                || (chunk_.isScript && !slot->everAssigned);
            if (useContextBase) {
                if (lhs->down->right != nullptr) {
                    if (slot.has_value() && slot->kind != SlotKind::Global) {
                        compileChainedIndexedAssign(lhs, *slot, printIt, true);
                    } else {
                        compileChainedIndexedAssign(lhs, std::nullopt, printIt);
                    }
                    return;
                }
                if (slot.has_value() && slot->kind != SlotKind::Global) {
                    emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                } else {
                    emit(OpCode::LOAD_GLOBAL, INST_FLAG_ALLOW_UNDEFINED, nameIndex(lhs->text));
                }
                push();
                compileExpr(rhs);
                compileStackValueAssign(lhs->down);
                if (slot.has_value() && slot->kind != SlotKind::Global) {
                    emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                    slots_.markAssigned(lhs->text);
                } else {
                    emit(OpCode::STORE_GLOBAL, 0, nameIndex(lhs->text));
                }
                pop();
                if (printIt) {
                    if (slot.has_value() && slot->kind != SlotKind::Global) {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                    } else {
                        emit(OpCode::LOAD_GLOBAL, 0, nameIndex(lhs->text));
                        push();
                        emit(OpCode::DUP);
                        push();
                        emit(OpCode::STORE_ANS);
                        pop();
                        emit(OpCode::DISPLAY_ANS);
                        emit(OpCode::POP);
                        pop();
                    }
                }
                return;
            }
            if (lhs->down->right != nullptr) {
                compileChainedIndexedAssign(lhs, *slot, printIt);
                return;
            }
            if ((lhs->down->opNum == OP_PARENS || lhs->down->opNum == OP_BRACES)
                && !containsEndKeyword(lhs->down)) {
                compileExpr(rhs);
                uint16_t nargs = 0;
                AbstractSyntaxTreePtr arg = lhs->down->down;
                while (arg != nullptr) {
                    compileAssignmentIndexExpr(arg);
                    ++nargs;
                    arg = arg->right;
                }
                emit(lhs->down->opNum == OP_PARENS ? OpCode::ASGN_PARENS : OpCode::ASGN_BRACES, 0,
                    slot->index, nargs);
                pop(static_cast<uint16_t>(nargs + 1));
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
                return;
            }

            emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
            push();
            compileExpr(rhs);
            compileStackValueAssign(lhs->down);
            emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
            pop();
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
            }
        }

        void
        compileChainedIndexedAssign(AbstractSyntaxTreePtr lhs, const SlotInfo& slot, bool printIt,
            bool loadContextBase = false)
        {
            compileChainedIndexedAssign(
                lhs, std::optional<SlotInfo>(slot), printIt, loadContextBase);
        }

        void
        compileChainedIndexedAssign(AbstractSyntaxTreePtr lhs, const std::optional<SlotInfo>& slot,
            bool printIt, bool loadContextBase = false)
        {
            std::vector<AbstractSyntaxTreePtr> parents;
            AbstractSyntaxTreePtr sub = lhs->down;
            while (sub != nullptr && sub->right != nullptr) {
                if ((sub->opNum != OP_PARENS && sub->opNum != OP_BRACES && sub->opNum != OP_DOT
                        && sub->opNum != OP_DOTDYN)
                    || sub->down == nullptr) {
                    unsupported(sub, "chained indexed assignment parent");
                }
                parents.push_back(sub);
                sub = sub->right;
            }
            if (sub == nullptr) {
                unsupported(lhs, "chained indexed assignment final");
            }

            if (slot.has_value() && !loadContextBase && !slot->everAssigned && parents.size() == 1
                && (parents[0]->opNum == OP_PARENS || parents[0]->opNum == OP_BRACES)
                && (sub->opNum == OP_DOT || sub->opNum == OP_DOTDYN)) {
                emit(OpCode::LOAD_EMPTY);
                push();
                emit(OpCode::LOAD_EMPTY);
                push();
                compileExpr(lhs->right);
                compileStackValueAssign(sub);
                compileStackValueAssign(parents[0]);
                emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                pop();
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
                return;
            }

            if (slot.has_value()) {
                emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
            } else {
                emit(OpCode::LOAD_GLOBAL, INST_FLAG_ALLOW_UNDEFINED, nameIndex(lhs->text));
            }
            push();
            for (AbstractSyntaxTreePtr parent : parents) {
                emit(OpCode::DUP);
                push();
                uint8_t parentFlags = INST_FLAG_ALLOW_UNDEFINED;
                if (parent->opNum == OP_PARENS) {
                    parentFlags |= INST_FLAG_INDEX_ONLY;
                }
                compileSubindex(parent, parentFlags);
            }

            compileExpr(lhs->right);
            compileStackValueAssign(sub);

            for (auto it = parents.rbegin(); it != parents.rend(); ++it) {
                compileStackValueAssign(*it);
            }

            if (slot.has_value()) {
                emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
            } else {
                emit(OpCode::STORE_GLOBAL, printIt ? INST_FLAG_PRINT : 0, nameIndex(lhs->text));
            }
            pop();
            if (slot.has_value()) {
                slots_.markAssigned(lhs->text);
            }
            if (printIt) {
                if (slot.has_value()) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                } else {
                    emit(OpCode::LOAD_GLOBAL, 0, nameIndex(lhs->text));
                    push();
                    emit(OpCode::DUP);
                    push();
                    emit(OpCode::STORE_ANS);
                    pop();
                    emit(OpCode::DISPLAY_ANS);
                    emit(OpCode::POP);
                    pop();
                }
            }
        }

        void
        compileStackValueAssign(AbstractSyntaxTreePtr sub)
        {
            uint16_t nargs = 0;
            switch (sub->opNum) {
            case OP_PARENS:
            case OP_BRACES: {
                AbstractSyntaxTreePtr arg = sub->down;
                uint16_t total = countPeers(arg);
                while (arg != nullptr) {
                    compileIndexExpr(arg, static_cast<uint16_t>(nargs + 1), nargs, total);
                    ++nargs;
                    arg = arg->right;
                }
                emit(
                    sub->opNum == OP_PARENS ? OpCode::ASGN_PARENS_VALUE : OpCode::ASGN_BRACES_VALUE,
                    0, 0, nargs);
                pop(static_cast<uint16_t>(nargs + 1));
                return;
            }
            case OP_DOT:
                if (sub->down == nullptr) {
                    unsupported(sub, "dot assignment");
                }
                emit(OpCode::ASGN_DOT_VALUE, 0, 0, chunk_.constants.addName(sub->down->text));
                pop();
                return;
            case OP_DOTDYN:
                compileExpr(sub->down);
                emit(OpCode::ASGN_DOTDYN_VALUE);
                pop(2);
                return;
            default:
                unsupported(sub, "stack indexed assignment operator");
            }
        }

        void
        compileAssignmentIndexExpr(AbstractSyntaxTreePtr expr)
        {
            if (expr != nullptr && expr->type == non_terminal && expr->opNum == OP_ALL) {
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(":"));
                push();
                return;
            }
            if (expr != nullptr && expr->type == non_terminal && expr->opNum == OP_RHS
                && expr->down != nullptr && expr->down->type == id_node
                && expr->down->down != nullptr) {
                compileNamedCallOrIndex(expr->down, 1, false, true);
                return;
            }
            compileExpr(expr);
        }

        void
        compileSpecialCall(AbstractSyntaxTreePtr node, bool printIt)
        {
            if (node == nullptr || node->type != id_node) {
                unsupported(node, "special function call");
            }
            if (tryCompileFastLocalClear(node)) {
                return;
            }
            if (node->text == "global" || node->text == "persistent") {
                OpCode declareOp
                    = node->text == "global" ? OpCode::DECLARE_GLOBAL : OpCode::DECLARE_PERSISTENT;
                AbstractSyntaxTreePtr arg = node->right;
                while (arg != nullptr) {
                    emit(declareOp, 0, nameIndex(arg->text));
                    slots_.forceGlobal(arg->text);
                    arg = arg->right;
                }
                return;
            }
            uint16_t nargs = 0;
            AbstractSyntaxTreePtr arg = node->right;
            while (arg != nullptr) {
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(arg->text));
                push();
                ++nargs;
                arg = arg->right;
            }
            (void)printIt;
            emit(OpCode::CALL_NAMED, 0, nameIndex(node->text), nargs, 0);
            pop(nargs);
            if (node->text == "clear" && node->right == nullptr) {
                uint16_t totalSlots = slots_.totalSlots();
                for (uint16_t slot = 0; slot < totalSlots; ++slot) {
                    emit(OpCode::DELETE_LOCAL, 0, slot);
                }
                return;
            }
            if (node->text == "clear") {
                arg = node->right;
                while (arg != nullptr) {
                    if (arg->text != "all" && arg->text != "variables" && arg->text != "global"
                        && arg->text != "functions" && arg->text != "classes") {
                        auto slot = slots_.resolve(arg->text);
                        if (slot.has_value() && slot->kind != SlotKind::Global) {
                            emit(OpCode::DELETE_LOCAL, 0, slot->index);
                        }
                    }
                    arg = arg->right;
                }
            }
        }

        bool
        tryCompileFastLocalClear(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->text != "clear" || node->right == nullptr) {
                return false;
            }
            std::vector<uint16_t> slotsToClear;
            AbstractSyntaxTreePtr arg = node->right;
            while (arg != nullptr) {
                if (arg->text == "all" || arg->text == "variables" || arg->text == "global"
                    || arg->text == "functions" || arg->text == "classes") {
                    return false;
                }
                auto slot = slots_.resolve(arg->text);
                if (!slot.has_value() || slot->kind == SlotKind::Global || !slot->everAssigned) {
                    return false;
                }
                if (std::find(
                        activeLoopLocalSlots_.begin(), activeLoopLocalSlots_.end(), slot->index)
                    == activeLoopLocalSlots_.end()) {
                    return false;
                }
                slotsToClear.push_back(slot->index);
                arg = arg->right;
            }
            for (uint16_t slot : slotsToClear) {
                emit(OpCode::CLEAR_LOCAL, 0, slot);
            }
            return true;
        }

        void
        compileMultiCall(AbstractSyntaxTreePtr lhsRoot, bool printIt)
        {
            if (lhsRoot == nullptr || lhsRoot->opNum != OP_BRACKETS || lhsRoot->right == nullptr) {
                unsupported(lhsRoot, "multi-output call");
            }
            AbstractSyntaxTreePtr row = lhsRoot->down;
            if (row == nullptr || row->opNum != OP_SEMICOLON || row->right != nullptr) {
                unsupported(lhsRoot, "multi-output lhs");
            }

            std::vector<AbstractSyntaxTreePtr> lhsNodes;
            AbstractSyntaxTreePtr lhs = row->down;
            while (lhs != nullptr) {
                if (lhs->type == reserved_node) {
                    Error(_W("Syntax error."));
                }
                if (lhs->type != non_terminal || lhs->opNum != OP_RHS || lhs->down == nullptr
                    || lhs->down->type != id_node) {
                    Error(_W("Syntax error."));
                }
                lhsNodes.push_back(lhs->down);
                lhs = lhs->right;
            }
            if (lhsNodes.empty()) {
                unsupported(lhsRoot, "empty multi-output lhs");
            }

            AbstractSyntaxTreePtr callee = lhsRoot->right;
            if (callee->type != id_node) {
                unsupported(callee, "multi-output callee");
            }
            auto calleeSlot = slots_.resolve(callee->text);
            bool calleeIsLocal = calleeSlot.has_value() && calleeSlot->kind != SlotKind::Global
                && calleeSlot->everAssigned;
            if (calleeIsLocal) {
                emit(OpCode::LOAD_LOCAL, 0, calleeSlot->index);
                push();
            }
            uint16_t nargs = 0;
            if (callee->down != nullptr) {
                if (callee->down->opNum != OP_PARENS || callee->down->right != nullptr) {
                    unsupported(callee, "multi-output callee indexing");
                }
                AbstractSyntaxTreePtr arg = callee->down->down;
                while (arg != nullptr) {
                    compileCallArgument(arg);
                    ++nargs;
                    arg = arg->right;
                }
            }

            bool singleBraceCommaListTarget = lhsNodes.size() == 1 && lhsNodes[0]->down != nullptr
                && lhsNodes[0]->down->opNum == OP_BRACES;
            uint16_t nout = static_cast<uint16_t>(lhsNodes.size());
            constexpr uint16_t DYNAMIC_OUTPUT_STACK_RESERVE = 16;
            if (singleBraceCommaListTarget) {
                AbstractSyntaxTreePtr outputCountExpr = lhsNodes[0]->down->down;
                if (outputCountExpr == nullptr || outputCountExpr->right != nullptr) {
                    unsupported(lhsRoot, "dynamic multi-output brace lhs");
                }
                bool allTargetElements = outputCountExpr != nullptr
                    && outputCountExpr->type == non_terminal && outputCountExpr->opNum == OP_ALL;
                if (allTargetElements || isOneToEndExpr(outputCountExpr)) {
                    auto targetSlot = slots_.resolve(lhsNodes[0]->text);
                    if (!targetSlot.has_value() || targetSlot->kind == SlotKind::Global) {
                        emit(OpCode::LOAD_GLOBAL_COUNT_RANGE, INST_FLAG_ALLOW_UNDEFINED,
                            nameIndex(lhsNodes[0]->text));
                    } else {
                        emit(OpCode::LOAD_LOCAL_COUNT_RANGE, 0, targetSlot->index);
                    }
                    push();
                } else {
                    compileExpr(outputCountExpr);
                }
                if (calleeIsLocal) {
                    emit(OpCode::CALL_HANDLE_DYNAMIC, 0, 0, nargs, 0);
                    pop(static_cast<uint16_t>(nargs + 2));
                } else {
                    emit(OpCode::CALL_NAMED_DYNAMIC, 0, nameIndex(callee->text), nargs, 0);
                    pop(static_cast<uint16_t>(nargs + 1));
                }
                for (uint16_t k = 0; k < DYNAMIC_OUTPUT_STACK_RESERVE; ++k) {
                    push();
                }
            } else {
                if (calleeIsLocal) {
                    emit(OpCode::CALL_HANDLE, 0, 0, nargs, nout);
                    pop(static_cast<uint16_t>(nargs + 1));
                } else {
                    emit(OpCode::CALL_NAMED, 0, nameIndex(callee->text), nargs, nout);
                    pop(nargs);
                }
                for (uint16_t k = 0; k < nout; ++k) {
                    push();
                }
            }
            if (singleBraceCommaListTarget) {
                AbstractSyntaxTreePtr target = lhsNodes[0];
                auto slot = slots_.resolve(target->text);
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::LOAD_GLOBAL, INST_FLAG_ALLOW_UNDEFINED, nameIndex(target->text));
                } else {
                    emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                }
                push();
                uint16_t targetNargs = 0;
                AbstractSyntaxTreePtr arg = target->down->down;
                uint16_t total = countPeers(arg);
                while (arg != nullptr) {
                    compileIndexExpr(arg, targetNargs, targetNargs, total);
                    ++targetNargs;
                    arg = arg->right;
                }
                emit(OpCode::ASGN_BRACES_MULTI_VALUE, 0, 0, targetNargs, 0);
                pop(static_cast<uint16_t>(DYNAMIC_OUTPUT_STACK_RESERVE + targetNargs));
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::STORE_GLOBAL, 0, nameIndex(target->text));
                } else {
                    emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                    slots_.markAssigned(target->text);
                    if (printIt) {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(target->text));
                    }
                }
                pop();
                return;
            }
            for (size_t k = lhsNodes.size(); k > 0; --k) {
                AbstractSyntaxTreePtr target = lhsNodes[k - 1];
                if (isPlaceholderIdentifier(target->text)) {
                    emit(OpCode::POP);
                    pop();
                    continue;
                }
                auto slot = slots_.resolve(target->text);
                if (target->down != nullptr) {
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::LOAD_GLOBAL, INST_FLAG_ALLOW_UNDEFINED,
                            nameIndex(target->text));
                    } else {
                        emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                    }
                    push();
                    emit(OpCode::SWAP);
                    compileStackValueAssign(target->down);
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::STORE_GLOBAL, 0, nameIndex(target->text));
                    } else {
                        emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                        slots_.markAssigned(target->text);
                        if (printIt) {
                            emit(OpCode::DISPLAY, 0, slot->index, nameIndex(target->text));
                        }
                    }
                    pop();
                    continue;
                }
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::STORE_GLOBAL, 0, nameIndex(target->text));
                } else {
                    emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                    slots_.markAssigned(target->text);
                    if (printIt) {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(target->text));
                    }
                }
                pop();
            }
        }

        void
        compileExpr(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr) {
                unsupported(node, "null expression");
            }
            switch (node->type) {
            case const_double_node:
            case const_int_node:
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addDouble(textToDouble(node->text)));
                push();
                return;
            case const_float_node:
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addSingle(textToSingle(node->text)));
                push();
                return;
            case const_uint8_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::uint8Constructor(textToUint8(node->text))));
                push();
                return;
            case const_int8_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int8Constructor(textToInt8(node->text))));
                push();
                return;
            case const_uint16_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        ArrayOf::uint16Constructor(textToUint16(node->text))));
                push();
                return;
            case const_int16_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int16Constructor(textToInt16(node->text))));
                push();
                return;
            case const_uint32_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        ArrayOf::uint32Constructor(textToUint32(node->text))));
                push();
                return;
            case const_int32_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int32Constructor(textToInt32(node->text))));
                push();
                return;
            case const_uint64_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        ArrayOf::uint64Constructor(textToUint64(node->text))));
                push();
                return;
            case const_int64_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int64Constructor(textToInt64(node->text))));
                push();
                return;
            case const_character_array_node:
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(node->text));
                push();
                return;
            case const_string_node:
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addStringObj(node->text));
                push();
                return;
            case const_complex_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        decomplexify(ArrayOf::complexConstructor(0, textToSingle(node->text)))));
                push();
                return;
            case const_dcomplex_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        decomplexify(ArrayOf::dcomplexConstructor(0, textToDouble(node->text)))));
                push();
                return;
            case reserved_node:
                if (node->tokenNumber == NLS_KEYWORD_END) {
                    emit(OpCode::LOAD_END);
                    push();
                    return;
                }
                unsupported(node, "reserved expression");
            case id_node:
                compileVarLoad(node);
                return;
            case non_terminal:
                compileOperatorExpr(node);
                return;
            default:
                unsupported(node, "expression node");
            }
        }

        void
        compileVarLoad(AbstractSyntaxTreePtr node)
        {
            if (node->text == "true") {
                emit(OpCode::LOAD_TRUE);
                push();
                return;
            }
            if (node->text == "false") {
                emit(OpCode::LOAD_FALSE);
                push();
                return;
            }
            auto slot = slots_.resolve(node->text);
            if (!slot.has_value() || slot->kind == SlotKind::Global || !slot->everAssigned) {
                emit(OpCode::LOAD_GLOBAL, 0, nameIndex(node->text));
            } else {
                emit(OpCode::LOAD_LOCAL, 0, slot->index);
            }
            push();
        }

        void
        compileOperatorExpr(AbstractSyntaxTreePtr node)
        {
            switch (node->opNum) {
            case OP_RHS:
                if (node->down == nullptr) {
                    unsupported(node, "complex rhs");
                }
                if (node->down->down != nullptr) {
                    compileNamedCallOrIndex(node->down, 1);
                    return;
                }
                compileVarLoad(node->down);
                return;
            case OP_EMPTY:
                emit(OpCode::LOAD_EMPTY);
                push();
                return;
            case OP_EMPTY_CELL:
                emit(OpCode::LOAD_EMPTY_CELL);
                push();
                return;
            case OP_COLON:
                compileColon(node);
                return;
            case OP_BRACKETS:
                if (node->down == nullptr) {
                    emit(OpCode::LOAD_EMPTY);
                    push();
                    return;
                }
                compileMatrixLiteral(node);
                return;
            case OP_BRACES:
                compileCellLiteral(node);
                return;
            case OP_FUNCTION_HANDLE_NAMED:
                if (node->down == nullptr) {
                    unsupported(node, "named function handle");
                }
                emit(OpCode::MAKE_FH_NAMED, 0, nameIndex(node->down->text));
                push();
                return;
            case OP_FUNCTION_HANDLE_ANONYMOUS:
                compileAnonymousFunctionHandle(node);
                return;
            case OP_POSTFIX:
                compilePostfixExpr(node);
                return;
            case OP_PLUS:
            case OP_SUBTRACT:
            case OP_TIMES:
            case OP_DOT_TIMES:
            case OP_RDIV:
            case OP_DOT_RDIV:
            case OP_LDIV:
            case OP_DOT_LDIV:
            case OP_MPOWER:
            case OP_POWER:
            case OP_LT:
            case OP_LEQ:
            case OP_GT:
            case OP_GEQ:
            case OP_EQ:
            case OP_NEQ:
            case OP_AND:
            case OP_OR:
                compileExpr(node->down);
                compileExpr(node->down->right);
                emit(binaryOpcode(node->opNum));
                replaceBinary();
                return;
            case OP_UPLUS:
            case OP_UMINUS:
            case OP_NOT:
            case OP_TRANSPOSE:
            case OP_DOT_TRANSPOSE:
                compileExpr(node->down);
                emit(unaryOpcode(node->opNum));
                return;
            case OP_SAND:
            case OP_SOR:
                compileShortCircuit(node);
                return;
            default:
                unsupported(node, "operator expression");
            }
        }

        void
        compilePostfixExpr(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->down == nullptr) {
                unsupported(node, "postfix expression");
            }
            AbstractSyntaxTreePtr base = node->down;
            AbstractSyntaxTreePtr sub = base->right;
            if (sub == nullptr) {
                compileExpr(base);
                return;
            }
            compileExpr(base);
            while (sub != nullptr) {
                uint8_t flags = 0;
                if (sub->opNum == OP_PARENS && sub->right != nullptr) {
                    flags |= INST_FLAG_INDEX_ONLY;
                }
                if (sub->opNum == OP_DOT && sub->right != nullptr
                    && sub->right->opNum == OP_PARENS) {
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileIndexExpr(arg, nargs, nargs, countPeers(sub->right->down));
                        ++nargs;
                        arg = arg->right;
                    }
                    emit(OpCode::SUBIDX_DOT, flags, nargs,
                        chunk_.constants.addName(sub->down->text), 1);
                    pop(nargs);
                    sub = sub->right->right;
                    continue;
                }
                compileSubindex(sub, flags);
                sub = sub->right;
            }
        }

        void
        compileAnonymousFunctionHandle(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->down == nullptr) {
                unsupported(node, "anonymous function handle");
            }
            stringVector args;
            AbstractSyntaxTreePtr code = nullptr;
            if (node->down->right == nullptr) {
                code = node->down;
            } else {
                args = node->down->toStringList();
                code = node->down->right;
            }
            if (code == nullptr) {
                unsupported(node, "anonymous function body");
            }
            uint16_t contentIdx = chunk_.constants.addName(code->toString(true));
            uint16_t argsIdx = chunk_.constants.addName(joinStrings(args));
            emit(OpCode::MAKE_FH_ANONYMOUS, 0, contentIdx, argsIdx);
            push();
        }

        void
        compileCallArgument(AbstractSyntaxTreePtr node)
        {
            if (node != nullptr && node->type == non_terminal && node->opNum == OP_RHS
                && node->down != nullptr && node->down->type == id_node
                && node->down->down != nullptr) {
                if (chainContainsParens(node->down->down)) {
                    compileExpr(node);
                } else {
                    compileNamedCallOrIndex(node->down, 1, false, true);
                }
                return;
            }
            compileExpr(node);
            if (node != nullptr && node->type == non_terminal && node->opNum == OP_RHS
                && node->down != nullptr && node->down->type == id_node
                && node->down->down == nullptr) {
                emit(OpCode::SET_NAME, 0, nameIndex(node->down->text));
            }
        }

        bool
        chainContainsParens(AbstractSyntaxTreePtr sub) const
        {
            while (sub != nullptr) {
                if (sub->opNum == OP_PARENS) {
                    return true;
                }
                sub = sub->right;
            }
            return false;
        }

        AbstractSyntaxTreePtr
        indexedIdentifier(AbstractSyntaxTreePtr node) const
        {
            if (node == nullptr) {
                return nullptr;
            }
            if (node->type == id_node && node->down != nullptr) {
                return node;
            }
            if (node->type == non_terminal && node->opNum == OP_RHS && node->down != nullptr
                && node->down->type == id_node && node->down->down != nullptr) {
                return node->down;
            }
            return nullptr;
        }

        void
        compileNamedCallOrIndex(AbstractSyntaxTreePtr ident, int nout, bool outputOptional = false,
            bool commaList = false)
        {
            if (ident == nullptr || ident->type != id_node || ident->down == nullptr) {
                unsupported(ident, "named call");
            }
            AbstractSyntaxTreePtr sub = ident->down;
            auto slot = slots_.resolve(ident->text);
            bool definitelyLocal
                = slot.has_value() && slot->kind != SlotKind::Global && slot->everAssigned;
            bool explicitArgumentCountCall
                = (ident->text == "nargin" || ident->text == "nargout") && sub->opNum == OP_PARENS;
            bool logicalConstructorKeyword = !definitelyLocal
                && (ident->text == "true" || ident->text == "false") && sub->opNum == OP_PARENS;
            if (sub->opNum == OP_PARENS && sub->right == nullptr
                && (!definitelyLocal || explicitArgumentCountCall) && !containsAllIndex(sub->down)
                && (!containsDirectEndKeyword(sub->down)
                    || allowsDirectEndAsFunctionArgument(ident->text))
                && !logicalConstructorKeyword) {
                uint16_t nargs = 0;
                AbstractSyntaxTreePtr arg = sub->down;
                while (arg != nullptr) {
                    compileCallArgument(arg);
                    ++nargs;
                    arg = arg->right;
                }
                emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                    nameIndex(ident->text), nargs, static_cast<uint16_t>(nout));
                pop(nargs);
                if (nout > 0) {
                    push();
                }
                return;
            }

            bool firstSubindex = true;
            if (sub->opNum == OP_PARENS && sub->right != nullptr && !definitelyLocal
                && !containsAllIndex(sub->down)
                && (!containsDirectEndKeyword(sub->down)
                    || allowsDirectEndAsFunctionArgument(ident->text))
                && !logicalConstructorKeyword) {
                uint16_t nargs = 0;
                AbstractSyntaxTreePtr arg = sub->down;
                while (arg != nullptr) {
                    compileCallArgument(arg);
                    ++nargs;
                    arg = arg->right;
                }
                emit(OpCode::CALL_NAMED, 0, nameIndex(ident->text), nargs, 1);
                pop(nargs);
                push();
                sub = sub->right;
                firstSubindex = false;
            } else if (!definitelyLocal && sub->opNum == OP_DOT && sub->down != nullptr
                && sub->down->type == id_node) {
                const std::string memberName = sub->down->text;
                std::string classdefFunctionName;
                const std::string packageClassName = ident->text + "." + memberName;
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && sub->right->right == nullptr
                    && ClassdefDefinitionManager::getInstance()->loadClass(packageClassName)) {
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileCallArgument(arg);
                        ++nargs;
                        arg = arg->right;
                    }
                    emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                        nameIndex(packageClassName), nargs, static_cast<uint16_t>(nout));
                    pop(nargs);
                    if (nout > 0) {
                        push();
                    }
                    return;
                }
                if (sub->right != nullptr && sub->right->opNum == OP_DOT
                    && sub->right->down != nullptr && sub->right->down->type == id_node) {
                    const std::string packageMemberName = sub->right->down->text;
                    if (sub->right->right != nullptr && sub->right->right->opNum == OP_PARENS
                        && sub->right->right->right == nullptr
                        && ClassdefDefinitionManager::getInstance()->resolveStaticMethodFunction(
                            packageClassName, packageMemberName, classdefFunctionName,
                            chunk_.classdefAccessClassName)) {
                        uint16_t nargs = 0;
                        AbstractSyntaxTreePtr arg = sub->right->right->down;
                        while (arg != nullptr) {
                            compileCallArgument(arg);
                            ++nargs;
                            arg = arg->right;
                        }
                        emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                            nameIndex(classdefFunctionName), nargs, static_cast<uint16_t>(nout));
                        pop(nargs);
                        if (nout > 0) {
                            push();
                        }
                        return;
                    }
                    if (sub->right->right != nullptr && sub->right->right->opNum == OP_PARENS
                        && sub->right->right->right == nullptr
                        && looksLikeRuntimeClassdefReference(packageClassName)) {
                        uint16_t nargs = 0;
                        AbstractSyntaxTreePtr arg = sub->right->right->down;
                        while (arg != nullptr) {
                            compileCallArgument(arg);
                            ++nargs;
                            arg = arg->right;
                        }
                        emit(OpCode::CALL_CLASSDEF_MEMBER,
                            (outputOptional ? INST_FLAG_PRINT : 0) | INST_FLAG_INDEX_ONLY,
                            chunk_.constants.addName(packageClassName + "." + packageMemberName),
                            nargs, static_cast<uint16_t>(nout));
                        pop(nargs);
                        if (nout > 0) {
                            push();
                        }
                        return;
                    }
                    if (sub->right->right == nullptr && nout > 0) {
                        bool resolvedPackageClassMember
                            = ClassdefDefinitionManager::getInstance()
                                  ->resolveConstantPropertyFunction(packageClassName,
                                      packageMemberName, classdefFunctionName,
                                      chunk_.classdefAccessClassName)
                            || ClassdefDefinitionManager::getInstance()
                                   ->resolveEnumerationMemberFunction(
                                       packageClassName, packageMemberName, classdefFunctionName);
                        if (resolvedPackageClassMember) {
                            emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                                nameIndex(classdefFunctionName), 0, static_cast<uint16_t>(nout));
                            push();
                            return;
                        }
                        if (looksLikeRuntimeClassdefReference(packageClassName)) {
                            emit(OpCode::CALL_CLASSDEF_MEMBER, outputOptional ? INST_FLAG_PRINT : 0,
                                chunk_.constants.addName(
                                    packageClassName + "." + packageMemberName),
                                0, static_cast<uint16_t>(nout));
                            push();
                            return;
                        }
                    }
                }
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && sub->right->right == nullptr
                    && ClassdefDefinitionManager::getInstance()->resolveStaticMethodFunction(
                        ident->text, memberName, classdefFunctionName,
                        chunk_.classdefAccessClassName)) {
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileCallArgument(arg);
                        ++nargs;
                        arg = arg->right;
                    }
                    emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                        nameIndex(classdefFunctionName), nargs, static_cast<uint16_t>(nout));
                    pop(nargs);
                    if (nout > 0) {
                        push();
                    }
                    return;
                }
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && sub->right->right == nullptr
                    && looksLikeRuntimeClassdefReference(ident->text)) {
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileCallArgument(arg);
                        ++nargs;
                        arg = arg->right;
                    }
                    emit(OpCode::CALL_CLASSDEF_MEMBER,
                        (outputOptional ? INST_FLAG_PRINT : 0) | INST_FLAG_INDEX_ONLY,
                        chunk_.constants.addName(ident->text + "." + memberName), nargs,
                        static_cast<uint16_t>(nout));
                    pop(nargs);
                    if (nout > 0) {
                        push();
                    }
                    return;
                }
                if (sub->right == nullptr && nout > 0) {
                    bool resolvedClassMember
                        = ClassdefDefinitionManager::getInstance()->resolveConstantPropertyFunction(
                              ident->text, memberName, classdefFunctionName,
                              chunk_.classdefAccessClassName)
                        || ClassdefDefinitionManager::getInstance()
                               ->resolveEnumerationMemberFunction(
                                   ident->text, memberName, classdefFunctionName);
                    if (resolvedClassMember) {
                        emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                            nameIndex(classdefFunctionName), 0, static_cast<uint16_t>(nout));
                        push();
                        return;
                    }
                    if (looksLikeRuntimeClassdefReference(ident->text)) {
                        emit(OpCode::CALL_CLASSDEF_MEMBER, outputOptional ? INST_FLAG_PRINT : 0,
                            chunk_.constants.addName(ident->text + "." + memberName), 0,
                            static_cast<uint16_t>(nout));
                        push();
                        return;
                    }
                }
                compileVarLoad(ident);
            } else {
                compileVarLoad(ident);
            }
            bool hadElementSelection = false;
            while (sub != nullptr) {
                int currentSubOp = sub->opNum;
                uint8_t flags = 0;
                if (firstSubindex && (sub->opNum == OP_PARENS || sub->opNum == OP_BRACES)) {
                    if (nout == 0) {
                        flags = INST_FLAG_QUIET;
                    } else if (outputOptional) {
                        flags = INST_FLAG_PRINT;
                    }
                }
                if (commaList && sub->right == nullptr
                    && (sub->opNum == OP_BRACES || sub->opNum == OP_DOT
                        || sub->opNum == OP_DOTDYN)) {
                    flags |= INST_FLAG_LIST;
                }
                if (sub->opNum == OP_PARENS && sub->right != nullptr) {
                    flags |= INST_FLAG_INDEX_ONLY;
                }
                if (sub->opNum == OP_DOT && sub->right != nullptr
                    && sub->right->opNum == OP_PARENS) {
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileIndexExpr(arg, nargs, nargs, countPeers(sub->right->down));
                        ++nargs;
                        arg = arg->right;
                    }
                    emit(OpCode::SUBIDX_DOT, flags, nargs,
                        chunk_.constants.addName(sub->down->text), 1);
                    pop(nargs);
                    firstSubindex = false;
                    sub = sub->right->right;
                    continue;
                }
                compileSubindex(
                    sub, flags, (firstSubindex && logicalConstructorKeyword) ? uint16_t { 1 } : 0);
                if (currentSubOp == OP_PARENS || currentSubOp == OP_BRACES) {
                    hadElementSelection = true;
                }
                firstSubindex = false;
                sub = sub->right;
            }
        }

        bool
        containsAllIndex(AbstractSyntaxTreePtr arg)
        {
            while (arg != nullptr) {
                if (arg->type == non_terminal && arg->opNum == OP_ALL) {
                    return true;
                }
                arg = arg->right;
            }
            return false;
        }

        void
        compileSubindex(AbstractSyntaxTreePtr sub, uint8_t flags = 0, uint16_t extra = 0)
        {
            if (sub == nullptr) {
                unsupported(sub, "subindex");
            }
            uint16_t nargs = 0;
            switch (sub->opNum) {
            case OP_PARENS:
            case OP_BRACES: {
                AbstractSyntaxTreePtr arg = sub->down;
                uint16_t total = countPeers(arg);
                while (arg != nullptr) {
                    compileIndexExpr(arg, nargs, nargs, total);
                    ++nargs;
                    arg = arg->right;
                }
                emit(sub->opNum == OP_PARENS ? OpCode::SUBIDX_PARENS : OpCode::SUBIDX_BRACES, flags,
                    0, nargs, extra);
                pop(nargs);
                return;
            }
            case OP_DOT:
                if (sub->down == nullptr) {
                    unsupported(sub, "dot index");
                }
                emit(OpCode::SUBIDX_DOT, flags, 0, chunk_.constants.addName(sub->down->text));
                return;
            case OP_DOTDYN:
                compileExpr(sub->down);
                emit(OpCode::SUBIDX_DOTDYN, flags);
                pop();
                return;
            default:
                unsupported(sub, "subindex operator");
            }
        }

        void
        compileIndexExpr(AbstractSyntaxTreePtr expr, uint16_t baseDepth, uint16_t indexPosition,
            uint16_t indexCount)
        {
            emit(OpCode::PUSH_END_CTX, 0, baseDepth, indexPosition, indexCount);
            if (expr != nullptr && expr->type == non_terminal && expr->opNum == OP_ALL) {
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(":"));
                push();
                emit(OpCode::POP_END_CTX);
                return;
            }
            if (expr != nullptr && expr->type == non_terminal && expr->opNum == OP_RHS
                && expr->down != nullptr && expr->down->type == id_node
                && expr->down->down != nullptr) {
                compileNamedCallOrIndex(expr->down, 1, false, true);
                emit(OpCode::POP_END_CTX);
                return;
            }
            compileExpr(expr);
            emit(OpCode::POP_END_CTX);
        }

        bool
        isDefinitelyAssignedLocal(const std::string& name)
        {
            auto slot = slots_.resolve(name);
            return slot.has_value() && slot->kind != SlotKind::Global && slot->everAssigned;
        }

        bool
        isIndexExpression(AbstractSyntaxTreePtr sub)
        {
            if (sub == nullptr) {
                return false;
            }
            if (sub->opNum == OP_BRACES || sub->opNum == OP_DOT || sub->opNum == OP_DOTDYN) {
                return true;
            }
            return sub->opNum == OP_PARENS && containsAllIndex(sub->down);
        }

        bool
        shouldDisplayIndexedRhs(AbstractSyntaxTreePtr ident)
        {
            if (ident == nullptr || ident->down == nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr sub = ident->down;
            if (isIndexExpression(sub)) {
                return true;
            }
            return sub->opNum == OP_PARENS && sub->right != nullptr && !containsAllIndex(sub->down);
        }

        void
        compileMatrixLiteral(AbstractSyntaxTreePtr node)
        {
            uint16_t nrows = 0;
            AbstractSyntaxTreePtr row = node->down;
            while (row != nullptr) {
                if (row->opNum != OP_SEMICOLON) {
                    unsupported(row, "matrix row");
                }
                uint16_t ncols = countPeers(row->down);
                AbstractSyntaxTreePtr value = row->down;
                while (value != nullptr) {
                    if (AbstractSyntaxTreePtr ident = indexedIdentifier(value)) {
                        compileNamedCallOrIndex(ident, 1, false, true);
                    } else {
                        compileExpr(value);
                    }
                    value = value->right;
                }
                emit(OpCode::BUILD_ROW, 0, ncols);
                pop(ncols);
                push();
                ++nrows;
                row = row->right;
            }
            emit(OpCode::BUILD_MATRIX, 0, nrows);
            pop(nrows);
            push();
        }

        void
        compileCellLiteral(AbstractSyntaxTreePtr node)
        {
            if (tryCompileCellFieldList(node)) {
                return;
            }

            uint16_t nrows = 0;
            uint16_t expectedCols = 0;
            AbstractSyntaxTreePtr row = node->down;
            while (row != nullptr) {
                if (row->opNum != OP_SEMICOLON) {
                    unsupported(row, "cell row");
                }
                uint16_t ncols = countPeers(row->down);
                if (nrows == 0) {
                    expectedCols = ncols;
                } else if (ncols != expectedCols) {
                    unsupported(row, "ragged cell literal");
                }
                AbstractSyntaxTreePtr value = row->down;
                while (value != nullptr) {
                    if (AbstractSyntaxTreePtr ident = indexedIdentifier(value)) {
                        compileNamedCallOrIndex(ident, 1, false, true);
                    } else {
                        compileExpr(value);
                    }
                    value = value->right;
                }
                ++nrows;
                row = row->right;
            }
            emit(OpCode::BUILD_CELL, 0, nrows, expectedCols);
            pop(static_cast<uint16_t>(nrows * expectedCols));
            push();
        }

        bool
        tryCompileCellFieldList(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr || node->down == nullptr || node->down->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr row = node->down;
            if (row->opNum != OP_SEMICOLON || row->down == nullptr || row->down->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr value = row->down;
            if (value->type != non_terminal || value->opNum != OP_RHS || value->down == nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr ident = value->down;
            if (ident->type != id_node || ident->down == nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr fieldSub = ident->down;
            while (fieldSub->right != nullptr) {
                fieldSub = fieldSub->right;
            }
            if (fieldSub->opNum != OP_DOT || fieldSub->down == nullptr) {
                return false;
            }

            compileVarLoad(ident);
            AbstractSyntaxTreePtr sub = ident->down;
            while (sub != fieldSub) {
                compileSubindex(sub);
                sub = sub->right;
            }
            emit(OpCode::BUILD_CELL_FIELD_LIST, 0, 0,
                chunk_.constants.addName(fieldSub->down->text));
            return true;
        }

        void
        compileShortCircuit(AbstractSyntaxTreePtr node)
        {
            compileExpr(node->down);
            uint32_t jump = 0;
            uint32_t jumpEnd = 0;
            if (node->opNum == OP_SAND) {
                jump = emit(OpCode::JUMP_IF_FALSE_SCALAR);
                pop();
                compileExpr(node->down->right);
                emit(OpCode::CHECK_SCALAR_AND);
                jumpEnd = emit(OpCode::JUMP);
                uint32_t falsePC = static_cast<uint32_t>(chunk_.code.size());
                chunk_.patchJump(jump, static_cast<uint16_t>(falsePC));
                stackDepth_ = 0;
                emit(OpCode::LOAD_FALSE);
                push();
            } else {
                jump = emit(OpCode::JUMP_IF_TRUE_SCALAR);
                pop();
                compileExpr(node->down->right);
                emit(OpCode::CHECK_SCALAR_OR);
                jumpEnd = emit(OpCode::JUMP);
                uint32_t truePC = static_cast<uint32_t>(chunk_.code.size());
                chunk_.patchJump(jump, static_cast<uint16_t>(truePC));
                stackDepth_ = 0;
                emit(OpCode::LOAD_TRUE);
                push();
            }
            chunk_.patchJump(jumpEnd, static_cast<uint16_t>(chunk_.code.size()));
            stackDepth_ = 1;
        }

        void
        compileColon(AbstractSyntaxTreePtr node)
        {
            if (node->down == nullptr || node->down->right == nullptr) {
                unsupported(node, "colon expression");
            }
            if (node->down->opNum == OP_COLON) {
                AbstractSyntaxTreePtr inner = node->down;
                compileExpr(inner->down);
                compileExpr(inner->down->right);
                compileExpr(inner->right);
                emit(OpCode::OP_COLON);
                pop(3);
                push();
            } else {
                compileExpr(node->down);
                compileExpr(node->down->right);
                emit(OpCode::OP_COLON_UNIT);
                replaceBinary();
            }
        }
    };
    //=============================================================================
    std::unique_ptr<BytecodeChunk>
    makeChunk(const std::string& functionName, const std::wstring& sourcePath, bool isScript,
        const stringVector& argNames, const stringVector& retNames,
        const stringVector& capturedNames = stringVector(),
        const std::string& classdefAccessClassName = std::string())
    {
        auto chunk = std::make_unique<BytecodeChunk>();
        chunk->functionName = functionName;
        chunk->classdefAccessClassName = classdefAccessClassName;
        chunk->sourcePath = sourcePath;
        chunk->isScript = isScript;
        chunk->argNames = argNames;
        chunk->retNames = retNames;
        chunk->capturedNames = capturedNames;
        return chunk;
    }
    //=============================================================================
    void
    compileInto(BytecodeChunk& chunk, AbstractSyntaxTreePtr body)
    {
        SlotMap slots(chunk.isScript);
        for (size_t k = 0; k < chunk.argNames.size(); ++k) {
            slots.addArg(chunk.argNames[k], static_cast<uint16_t>(k));
        }
        CompilerContext compiler(chunk, slots);
        compiler.compileBlock(body);
        chunk.retSlots.clear();
        chunk.retSlots.reserve(chunk.retNames.size());
        for (const std::string& retName : chunk.retNames) {
            auto slot = slots.resolve(retName);
            if (slot.has_value() && slot->kind != SlotKind::Global) {
                chunk.retSlots.push_back(slot->index);
            }
        }
        chunk.localNames = slots.slotNames();
        chunk.localAssigned = slots.slotAssigned();
        chunk.nLocals = std::max(chunk.nLocals, slots.totalSlots());
    }
    //=============================================================================
} // namespace
//=============================================================================
std::unique_ptr<BytecodeChunk>
BytecodeCompiler::compileFunction(AbstractSyntaxTreePtr body, const std::string& functionName,
    const std::wstring& sourcePath, const stringVector& argNames, const stringVector& retNames,
    const std::string& classdefAccessClassName)
{
    auto chunk = makeChunk(
        functionName, sourcePath, false, argNames, retNames, {}, classdefAccessClassName);
    compileInto(*chunk, body);
    return chunk;
}
//=============================================================================
std::unique_ptr<BytecodeChunk>
BytecodeCompiler::compileScript(AbstractSyntaxTreePtr body, const std::wstring& sourcePath)
{
    auto chunk = makeChunk("", sourcePath, true, stringVector(), stringVector());
    compileInto(*chunk, body);
    return chunk;
}
//=============================================================================
std::unique_ptr<BytecodeChunk>
BytecodeCompiler::compileAnonymous(AbstractSyntaxTreePtr body, const stringVector& capturedNames,
    const stringVector& argNames, const stringVector& retNames)
{
    auto chunk = makeChunk("Anonymous", L"", false, argNames, retNames, capturedNames);
    compileInto(*chunk, body);
    return chunk;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
