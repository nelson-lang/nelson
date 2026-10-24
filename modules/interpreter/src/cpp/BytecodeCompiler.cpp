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
#include <cmath>
#include <limits>
#include <stdexcept>
#include <unordered_set>
#include "BytecodeCompiler.hpp"
#include "BytecodeScalarFusion.hpp"
#include "BuiltinEmptyTypeName.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Keywords.hpp"
#include "LexerContext.hpp"
#include "PathFunctionIndexerManager.hpp"
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
        std::string message
            = formatI18nMessage(Nelson::resolveMessageTemplate(
                                    "Nelson:interpreter:bytecodeCompilerUnsupportedConstruct",
                                    "Bytecode compiler unsupported construct: %s"),
                detail);
        Error("Nelson:bytecode:unsupportedConstruct", message);
        // Error() returns instead of throwing when no evaluator is registered.
        throw std::runtime_error(message);
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
    std::string
    classdefReferencePrefix(const std::string& baseName, AbstractSyntaxTreePtr sub)
    {
        if (baseName.empty()) {
            return {};
        }
        std::string prefix = baseName;
        std::string resolvedPrefix;
        AbstractSyntaxTreePtr current = sub;
        while (current != nullptr && current->opNum == OP_DOT) {
            if (current->down == nullptr || current->down->type != id_node) {
                return {};
            }
            std::string candidate = prefix + "." + current->down->text;
            if (classdefLoadClass(candidate)) {
                resolvedPrefix = candidate;
            }
            prefix = candidate;
            current = current->right;
        }
        return resolvedPrefix;
    }
    //=============================================================================
    AbstractSyntaxTreePtr
    subindexForQualifiedName(AbstractSyntaxTreePtr sub, const std::string& qualifiedName)
    {
        const size_t dotCount = std::count(qualifiedName.begin(), qualifiedName.end(), '.');
        AbstractSyntaxTreePtr current = sub;
        for (size_t k = 1; k < dotCount && current != nullptr; ++k) {
            current = current->right;
        }
        return current;
    }
    //=============================================================================
    bool
    looksLikeRuntimeClassdefReference(const std::string& className)
    {
        std::string leaf = classNameLeaf(className);
        if (leaf.empty()) {
            return false;
        }
        // An uppercase leading letter is a cheap hint that a name is a class, so classes added to
        // the path after this code is compiled still resolve at runtime. Lowercase class names are
        // not covered, so confirm by loading the class. Then `lowercaseClass.staticMethod(...)`
        // dispatches as a classdef member, not a constructor call.
        if (std::isupper(static_cast<unsigned char>(leaf.front())) != 0) {
            return true;
        }
        return classdefLoadClass(className);
    }
    //=============================================================================
    bool
    isNullSubindex(AbstractSyntaxTreePtr node)
    {
        return node != nullptr && node->type == non_terminal && node->opNum == OP_NULL;
    }
    //=============================================================================
    bool
    hasNonNullRight(AbstractSyntaxTreePtr node)
    {
        AbstractSyntaxTreePtr current = node != nullptr ? node->right : nullptr;
        while (current != nullptr) {
            if (!isNullSubindex(current)) {
                return true;
            }
            current = current->right;
        }
        return false;
    }
    //=============================================================================
    AbstractSyntaxTreePtr
    lastNonNullSubindex(AbstractSyntaxTreePtr node)
    {
        AbstractSyntaxTreePtr last = nullptr;
        while (node != nullptr) {
            if (!isNullSubindex(node)) {
                last = node;
            }
            node = node->right;
        }
        return last;
    }
    //=============================================================================
    bool
    chainEndsWithDotCall(AbstractSyntaxTreePtr node)
    {
        while (node != nullptr) {
            if (!isNullSubindex(node) && node->opNum == OP_DOT && node->right != nullptr
                && node->right->opNum == OP_PARENS && !hasNonNullRight(node->right)) {
                return true;
            }
            node = node->right;
        }
        return false;
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

        // Names declared 'global' in an enclosing function are global here too, so a nested
        // function reads and writes the shared global, not an unseeded local. Call before
        // compileBlock.
        void
        seedInheritedGlobals(const stringVector& names)
        {
            for (const std::string& name : names) {
                slots_.forceGlobal(name);
                declaredGlobalNames_.insert(name);
                // Register the name as global in this function's runtime scope, as an explicit
                // 'global' statement does, so indexed assignment also resolves the shared global.
                emit(OpCode::DECLARE_GLOBAL, 0, nameIndex(name));
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
        std::unordered_set<std::string> declaredGlobalNames_;
        std::unordered_set<std::string> declaredPersistentNames_;

        bool
        isDeclaredGlobalName(const std::string& name) const
        {
            return declaredGlobalNames_.find(name) != declaredGlobalNames_.end();
        }

        bool
        isDeclaredPersistentName(const std::string& name) const
        {
            return declaredPersistentNames_.find(name) != declaredPersistentNames_.end();
        }

        uint8_t
        loadContextFlags(const std::string& name, uint8_t flags = 0) const
        {
            if (isDeclaredGlobalName(name)) {
                flags |= INST_FLAG_GLOBAL;
            }
            if (isDeclaredPersistentName(name)) {
                flags |= INST_FLAG_ALLOW_UNDEFINED;
            }
            return flags;
        }

        uint8_t
        storeContextFlags(const std::string& name, uint8_t flags = 0) const
        {
            if (isDeclaredGlobalName(name)) {
                flags |= INST_FLAG_GLOBAL;
            }
            return flags;
        }

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

        // A multi-level pure-index chain (only (), {} or .field levels, no fused .method(args))
        // whose head is a local variable. It may be dispatched whole to a subsref / Redefines*
        // object so the hook sees the full operation list. CHAIN_BEGIN decides at runtime by the
        // head's type, so wrapping is inert for other values.
        static bool
        chainIsCollectCapable(AbstractSyntaxTreePtr sub)
        {
            int levels = 0;
            for (AbstractSyntaxTreePtr s = sub; s != nullptr; s = s->right) {
                if (s->opNum == OP_DOT) {
                    if (s->down == nullptr || s->down->type != id_node) {
                        return false;
                    }
                    if (s->right != nullptr && s->right->opNum == OP_PARENS) {
                        return false; // fused .method(args) form, handled per-level
                    }
                } else if (s->opNum != OP_PARENS && s->opNum != OP_BRACES) {
                    return false;
                }
                ++levels;
            }
            return levels >= 2;
        }

        static bool
        indexArgContainsEnd(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr) {
                return false;
            }
            if (node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_END) {
                return true;
            }
            return containsEndKeyword(node->down);
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

        // True when `end` is used at this index level, including inside `2:end-1` or `end/2`, but
        // not inside a nested indexed variable or call (the `end` of `x(2:end)` used as an argument
        // belongs to x). Tells an indexed variable read apart from a function call.
        static bool
        containsEndKeywordThisLevel(AbstractSyntaxTreePtr node)
        {
            while (node != nullptr) {
                if (node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_END) {
                    return true;
                }
                // A nested indexed variable or call is an id_node with its own subindex chain. Its
                // subscripts resolve `end` against that base, so they are not part of this level.
                bool nestedIndexed = node->type == id_node && node->down != nullptr;
                if (!nestedIndexed && containsEndKeywordThisLevel(node->down)) {
                    return true;
                }
                node = node->right;
            }
            return false;
        }

        static bool
        dotParensIndexUsesEnd(AbstractSyntaxTreePtr sub)
        {
            return sub != nullptr && sub->opNum == OP_DOT && sub->right != nullptr
                && sub->right->opNum == OP_PARENS && containsEndKeyword(sub->right->down);
        }

        static bool
        isDotParentFinalIndex(const std::vector<AbstractSyntaxTreePtr>& parents,
            AbstractSyntaxTreePtr sub, bool allowDynamicField = false)
        {
            return parents.size() == 1 && parents[0] != nullptr
                && (parents[0]->opNum == OP_DOT
                    || (allowDynamicField && parents[0]->opNum == OP_DOTDYN))
                && parents[0]->down != nullptr && sub != nullptr
                && (sub->opNum == OP_PARENS || sub->opNum == OP_BRACES);
        }

        static bool
        isDotParentFinalIndexUsingEnd(
            const std::vector<AbstractSyntaxTreePtr>& parents, AbstractSyntaxTreePtr sub)
        {
            return isDotParentFinalIndex(parents, sub) && containsEndKeyword(sub->down);
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
            if (node->opNum == OP_ARGUMENTS) {
                // Arguments validation blocks are lowered before compilation
                // (MacroFunctionDef::ensureBytecodeCompiled). A node reaching here is misplaced:
                // emit nothing.
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
                if (printIt && shouldDisplayIndexedRhs(node->down)
                    && !isDottedFunctionCallStatement(node->down)) {
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
                case NLS_KEYWORD_KEYBOARD:
                    // Shares the span recorded by the statement's DEBUG_STMT.
                    emit(OpCode::KEYBOARD, 0, static_cast<uint16_t>(chunk_.spans.size() - 1));
                    return;
                default:
                    unsupported(node, "reserved statement");
                }
            }
            if (node->opNum == OP_SUPERCLASS_CALL) {
                // Statement context: a bare method@Super(...) wants no output; a
                // bare constructor obj@Super(...) still updates obj (leaves 1).
                int left = compileSuperclassCall(node, printIt ? 1 : 0);
                for (int i = 0; i < left; ++i) {
                    if (printIt) {
                        emit(OpCode::DUP);
                        push();
                        emit(OpCode::STORE_ANS);
                        pop();
                        emit(OpCode::DISPLAY_ANS);
                    }
                    emit(OpCode::POP);
                    pop();
                }
                return;
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

        // `if nargin CMP K` compiles to a dedicated jump so "nargin" never
        // enters the chunk names: a chunk that uses nargin generically is
        // excluded from the scope-free call fast paths. A variable injected
        // into the scope at runtime under that name is not consulted, unlike
        // the generic load path.
        bool
        tryCompileNarginComparisonJump(
            AbstractSyntaxTreePtr condition, std::vector<uint32_t>& falseJumps)
        {
            if (chunk_.isScript) {
                return false;
            }
            AbstractSyntaxTreePtr cmp = simpleSingleNode(condition);
            if (cmp == nullptr || cmp->type != non_terminal || cmp->down == nullptr
                || cmp->down->right == nullptr) {
                return false;
            }
            uint8_t cmpId;
            switch (cmp->opNum) {
            case OP_LT:
                cmpId = NARGIN_CMP_LT;
                break;
            case OP_LEQ:
                cmpId = NARGIN_CMP_LE;
                break;
            case OP_GT:
                cmpId = NARGIN_CMP_GT;
                break;
            case OP_GEQ:
                cmpId = NARGIN_CMP_GE;
                break;
            case OP_EQ:
                cmpId = NARGIN_CMP_EQ;
                break;
            case OP_NEQ:
                cmpId = NARGIN_CMP_NE;
                break;
            default:
                return false;
            }
            AbstractSyntaxTreePtr lhs = simpleLocalIdNode(cmp->down);
            if (lhs == nullptr || lhs->text != "nargin") {
                return false;
            }
            AbstractSyntaxTreePtr rhs = simpleSingleNode(cmp->down->right);
            if (rhs == nullptr || (rhs->type != const_int_node && rhs->type != const_double_node)) {
                return false;
            }
            double bound = textToDouble(rhs->text);
            if (bound < 0 || bound > 65535 || bound != std::floor(bound)) {
                return false;
            }
            auto narginSlot = slots_.peek("nargin");
            if (narginSlot.has_value() && narginSlot->everAssigned) {
                return false;
            }
            falseJumps.push_back(
                emit(OpCode::JUMP_IF_NARGIN_CMP, cmpId, 0, static_cast<uint16_t>(bound)));
            return true;
        }

        // Machine-generated argument checks (`if ~isa(x, 'cls')`,
        // `if ~isscalar(x)`) compile to a dedicated jump followed by the
        // generic condition as a fallback window: the VM skips the window on
        // the fast path and falls through to it whenever the predicate name
        // no longer resolves to the pristine builtin.
        bool
        tryCompileArgumentPredicateJump(
            AbstractSyntaxTreePtr condition, std::vector<uint32_t>& falseJumps)
        {
            AbstractSyntaxTreePtr node = simpleSingleNode(condition);
            if (node == nullptr) {
                return false;
            }
            bool negated = false;
            if (node->type == non_terminal && node->opNum == OP_NOT) {
                negated = true;
                node = simpleSingleNode(node->down);
                if (node == nullptr) {
                    return false;
                }
            }
            if (node->type != id_node || node->down == nullptr || node->down->opNum != OP_PARENS
                || node->down->right != nullptr) {
                return false;
            }
            uint8_t predId;
            if (node->text == "isa") {
                predId = ARG_PRED_ISA;
            } else if (node->text == "isscalar") {
                predId = ARG_PRED_ISSCALAR;
            } else {
                return false;
            }
            if (slots_.peek(node->text).has_value()) {
                return false;
            }
            AbstractSyntaxTreePtr arg1 = node->down->down;
            if (arg1 == nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr baseId = simpleLocalIdNode(arg1);
            if (baseId == nullptr) {
                return false;
            }
            auto baseSlot = slots_.resolve(baseId->text);
            if (!baseSlot.has_value() || baseSlot->kind == SlotKind::Global) {
                return false;
            }
            uint16_t classConstant = 0;
            if (predId == ARG_PRED_ISA) {
                AbstractSyntaxTreePtr arg2 = arg1->right;
                if (arg2 == nullptr || arg2->right != nullptr) {
                    return false;
                }
                AbstractSyntaxTreePtr literal = simpleSingleNode(arg2);
                if (literal == nullptr || literal->type != const_character_array_node) {
                    return false;
                }
                classConstant = chunk_.constants.addName(literal->text);
            } else if (arg1->right != nullptr) {
                return false;
            }
            // body runs when the condition is true: skip it (jump) when the
            // predicate disagrees with the (possibly negated) condition
            uint8_t flags = predId;
            if (negated) {
                flags |= ARG_PRED_JUMP_ON_TRUE;
            }
            uint32_t fusedIndex
                = emit(OpCode::JUMP_IF_ARG_PRED, flags, 0, baseSlot->index, classConstant);
            uint32_t windowStart = static_cast<uint32_t>(chunk_.code.size());
            compileExpr(condition);
            falseJumps.push_back(emit(OpCode::JUMP_IF_FALSE));
            pop();
            uint32_t windowLength = static_cast<uint32_t>(chunk_.code.size()) - windowStart;
            if (windowLength > ARG_PRED_WINDOW_MASK) {
                // window too large to encode: keep the generic path only
                chunk_.code[fusedIndex].op = OpCode::NOP;
                chunk_.code[fusedIndex].flags = 0;
            } else {
                chunk_.code[fusedIndex].flags = static_cast<uint8_t>(
                    flags | static_cast<uint8_t>(windowLength << ARG_PRED_WINDOW_SHIFT));
                falseJumps.push_back(fusedIndex);
            }
            return true;
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
                std::vector<uint32_t> falseJumps;
                if (!tryCompileNarginComparisonJump(condition, falseJumps)
                    && !tryCompileArgumentPredicateJump(condition, falseJumps)) {
                    compileExpr(condition);
                    falseJumps.push_back(emit(OpCode::JUMP_IF_FALSE));
                    pop();
                }
                compileBlock(codeBlock);
                exitJumps.push_back(emit(OpCode::JUMP));
                for (uint32_t falseJump : falseJumps) {
                    patch(falseJump, static_cast<uint32_t>(chunk_.code.size()));
                }
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
            if (node == nullptr) {
                return;
            }
            // node->right (the loop body) may be null (`while true; end`). The loop is still
            // compiled so the condition is evaluated on each iteration.
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
            auto slot = slots_.resolve(lhs->text);
            if (!slot.has_value() || slot->kind == SlotKind::Global) {
                compileExpr(rhs);
                emit(OpCode::STORE_GLOBAL, storeContextFlags(lhs->text), nameIndex(lhs->text));
                pop();
            } else {
                if (tryCompileLocalParensAssign(rhs, *slot, printIt)) {
                    slots_.markAssigned(lhs->text);
                    return;
                }
                if (tryCompileLocalPlusParensAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalPlusAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalPlusRdivAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalPlusConstantAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalPlusStackAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalPlusCellAssign(lhs, rhs, *slot, printIt)) {
                    slots_.markAssigned(lhs->text);
                    return;
                }
                compileExpr(rhs);
                emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
                pop();
            }
        }

        AbstractSyntaxTreePtr
        simpleSingleNode(AbstractSyntaxTreePtr node)
        {
            if (node == nullptr) {
                return nullptr;
            }
            if (node->type == non_terminal && node->opNum == OP_RHS && node->down != nullptr
                && node->down->right == nullptr) {
                return node->down;
            }
            return node;
        }

        AbstractSyntaxTreePtr
        simpleLocalIdNode(AbstractSyntaxTreePtr node)
        {
            node = simpleSingleNode(node);
            if (node == nullptr) {
                return nullptr;
            }
            if (node->type == id_node && node->down == nullptr && node->text != "true"
                && node->text != "false") {
                return node;
            }
            return nullptr;
        }

        bool
        positiveUInt8IndexLiteral(AbstractSyntaxTreePtr node, uint8_t& ordinal)
        {
            node = simpleSingleNode(node);
            if (node == nullptr
                || (node->type != const_int_node && node->type != const_double_node)) {
                return false;
            }
            double value = textToDouble(node->text);
            if (value < 1 || value > 255 || value != std::floor(value)) {
                return false;
            }
            ordinal = static_cast<uint8_t>(value);
            return true;
        }

        std::optional<SlotInfo>
        usableLocalSlot(AbstractSyntaxTreePtr node)
        {
            AbstractSyntaxTreePtr id = simpleLocalIdNode(node);
            if (id == nullptr) {
                return std::nullopt;
            }
            auto slot = slots_.resolve(id->text);
            if (!slot.has_value() || slot->kind == SlotKind::Global || !slot->everAssigned) {
                return std::nullopt;
            }
            return slot;
        }

        bool
        doubleLiteralNode(AbstractSyntaxTreePtr node, double& value)
        {
            node = simpleSingleNode(node);
            if (node == nullptr
                || (node->type != const_int_node && node->type != const_double_node)) {
                return false;
            }
            value = textToDouble(node->text);
            return true;
        }

        bool
        localSlotWithConstantOffset(AbstractSyntaxTreePtr node, uint16_t& indexSlot, double& offset)
        {
            node = simpleSingleNode(node);
            offset = 0.;
            if (auto slot = usableLocalSlot(node)) {
                indexSlot = slot->index;
                return true;
            }
            if (node == nullptr || node->type != non_terminal
                || (node->opNum != OP_PLUS && node->opNum != OP_SUBTRACT) || node->down == nullptr
                || node->down->right == nullptr || node->down->right->right != nullptr) {
                return false;
            }
            if (auto slot = usableLocalSlot(node->down)) {
                double rhs = 0.;
                if (!doubleLiteralNode(node->down->right, rhs)) {
                    return false;
                }
                indexSlot = slot->index;
                offset = node->opNum == OP_SUBTRACT ? -rhs : rhs;
                return true;
            }
            if (node->opNum == OP_PLUS) {
                double lhs = 0.;
                if (doubleLiteralNode(node->down, lhs)) {
                    if (auto slot = usableLocalSlot(node->down->right)) {
                        indexSlot = slot->index;
                        offset = lhs;
                        return true;
                    }
                }
            }
            return false;
        }

        bool
        localSlotMinusLocalOffset(
            AbstractSyntaxTreePtr node, uint16_t& indexSlot, uint16_t& offsetSlot)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->type != non_terminal || node->opNum != OP_SUBTRACT
                || node->down == nullptr || node->down->right == nullptr
                || node->down->right->right != nullptr) {
                return false;
            }
            auto index = usableLocalSlot(node->down);
            auto offset = usableLocalSlot(node->down->right);
            if (!index.has_value() || !offset.has_value()) {
                return false;
            }
            indexSlot = index->index;
            offsetSlot = offset->index;
            return true;
        }

        bool
        localBraceReadSlots(AbstractSyntaxTreePtr node, uint16_t& baseSlot, uint16_t& indexSlot)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->type != id_node || node->down == nullptr
                || node->down->opNum != OP_BRACES || node->down->right != nullptr
                || containsEndKeyword(node->down)) {
                return false;
            }
            AbstractSyntaxTreePtr index = node->down->down;
            if (index == nullptr || index->right != nullptr) {
                return false;
            }
            auto base = slots_.resolve(node->text);
            double offset = 0.;
            uint16_t resolvedIndexSlot = 0;
            if (!base.has_value() || base->kind == SlotKind::Global || !base->everAssigned
                || !localSlotWithConstantOffset(index, resolvedIndexSlot, offset) || offset != 0.) {
                return false;
            }
            baseSlot = base->index;
            indexSlot = resolvedIndexSlot;
            return true;
        }

        bool
        localParensReadSlots(AbstractSyntaxTreePtr node, uint16_t& baseSlot, uint16_t& indexSlot)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->type != id_node || node->down == nullptr
                || node->down->opNum != OP_PARENS || node->down->right != nullptr
                || containsEndKeyword(node->down)) {
                return false;
            }
            AbstractSyntaxTreePtr index = node->down->down;
            if (index == nullptr || index->right != nullptr) {
                return false;
            }
            auto base = slots_.resolve(node->text);
            auto indexLocal = usableLocalSlot(index);
            if (!base.has_value() || base->kind == SlotKind::Global || !base->everAssigned
                || !indexLocal.has_value()) {
                return false;
            }
            baseSlot = base->index;
            indexSlot = indexLocal->index;
            return true;
        }

        bool
        tryCompileLocalParensAssign(
            AbstractSyntaxTreePtr rhs, const SlotInfo& targetSlot, bool printIt)
        {
            if (printIt) {
                return false;
            }
            uint16_t baseSlot = 0;
            uint16_t indexSlot = 0;
            if (!localParensReadSlots(rhs, baseSlot, indexSlot)) {
                return false;
            }
            emit(OpCode::ASSIGN_PARENS_LOCAL, 0, targetSlot.index, baseSlot, indexSlot);
            return true;
        }

        bool
        tryCompileLocalPlusParensAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            if (printIt || !targetSlot.everAssigned) {
                return false;
            }
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr left = simpleLocalIdNode(expr->down);
            if (left == nullptr || left->text != lhs->text) {
                return false;
            }
            uint16_t baseSlot = 0;
            uint16_t indexSlot = 0;
            if (!localParensReadSlots(expr->down->right, baseSlot, indexSlot)) {
                return false;
            }
            emit(OpCode::ASSIGN_PLUS_PARENS_LOCAL, 0, targetSlot.index, baseSlot, indexSlot);
            slots_.markAssigned(lhs->text);
            return true;
        }

        bool
        tryCompileLocalPlusCellAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            if (printIt || !targetSlot.everAssigned) {
                return false;
            }
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr left = expr->down;
            AbstractSyntaxTreePtr right = expr->down->right;
            uint16_t baseSlot = 0;
            uint16_t indexSlot = 0;
            AbstractSyntaxTreePtr local = simpleLocalIdNode(left);
            if (local != nullptr && local->text == lhs->text
                && localBraceReadSlots(right, baseSlot, indexSlot)) {
                emit(OpCode::ASSIGN_PLUS_CELL_LOCAL, 0, targetSlot.index, baseSlot, indexSlot);
                return true;
            }
            local = simpleLocalIdNode(right);
            if (local != nullptr && local->text == lhs->text
                && localBraceReadSlots(left, baseSlot, indexSlot)) {
                emit(OpCode::ASSIGN_PLUS_CELL_LOCAL, 0, targetSlot.index, baseSlot, indexSlot);
                return true;
            }
            return false;
        }

        bool
        tryCompileLocalPlusAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            auto leftSlot = usableLocalSlot(expr->down);
            auto rightSlot = usableLocalSlot(expr->down->right);
            if (!leftSlot.has_value() || !rightSlot.has_value()) {
                return false;
            }
            if (leftSlot->index == targetSlot.index) {
                emit(OpCode::ASSIGN_PLUS_LOCAL, 0, targetSlot.index, rightSlot->index);
            } else if (rightSlot->index == targetSlot.index) {
                emit(OpCode::ASSIGN_PLUS_LOCAL, 0, targetSlot.index, leftSlot->index);
            } else {
                return false;
            }
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, targetSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalPlusRdivAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr left = simpleLocalIdNode(expr->down);
            if (left == nullptr || left->text != lhs->text) {
                return false;
            }
            AbstractSyntaxTreePtr rdiv = simpleSingleNode(expr->down->right);
            if (rdiv == nullptr || rdiv->type != non_terminal || rdiv->opNum != OP_RDIV
                || rdiv->down == nullptr || rdiv->down->right == nullptr
                || rdiv->down->right->right != nullptr) {
                return false;
            }
            auto numeratorSlot = usableLocalSlot(rdiv->down);
            auto denominatorSlot = usableLocalSlot(rdiv->down->right);
            if (numeratorSlot.has_value() && denominatorSlot.has_value()) {
                emit(OpCode::ASSIGN_PLUS_LOCAL_RDIV_LOCAL, 0, targetSlot.index,
                    numeratorSlot->index, denominatorSlot->index);
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, targetSlot.index, nameIndex(lhs->text));
                }
                return true;
            }
            double numerator = 0;
            if (!doubleLiteral(rdiv->down, numerator)) {
                return false;
            }
            if (!denominatorSlot.has_value()) {
                return false;
            }
            emit(OpCode::ASSIGN_PLUS_RDIV_LOCAL, 0, targetSlot.index, denominatorSlot->index,
                chunk_.constants.addDouble(numerator));
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, targetSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalPlusConstantAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr left = simpleLocalIdNode(expr->down);
            if (left == nullptr || left->text != lhs->text) {
                return false;
            }
            double increment = 0;
            if (!doubleLiteral(expr->down->right, increment)) {
                return false;
            }
            emit(OpCode::ASSIGN_PLUS_CONST, 0, targetSlot.index,
                chunk_.constants.addDouble(increment));
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, targetSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalPlusStackAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& targetSlot, bool printIt)
        {
            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr left = simpleLocalIdNode(expr->down);
            if (left == nullptr || left->text != lhs->text) {
                return false;
            }
            emit(OpCode::LOAD_LOCAL, 0, targetSlot.index);
            push();
            compileExpr(expr->down->right);
            emit(OpCode::ASSIGN_PLUS_STACK, 0, targetSlot.index);
            pop(2);
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, targetSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        constantValueIndex(AbstractSyntaxTreePtr node, uint16_t& valueIndex)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->right != nullptr) {
                return false;
            }
            switch (node->type) {
            case const_int_node:
            case const_double_node:
                valueIndex = chunk_.constants.addDouble(textToDouble(node->text));
                return true;
            case const_character_array_node:
                valueIndex = chunk_.constants.addString(node->text);
                return true;
            case const_string_node:
                valueIndex = chunk_.constants.addStringObj(node->text);
                return true;
            default:
                return false;
            }
        }

        bool
        tryCompileLocalBraceConstAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            if (lhs == nullptr || lhs->down == nullptr || lhs->down->opNum != OP_BRACES
                || lhs->down->right != nullptr || containsEndKeyword(lhs->down)) {
                return false;
            }
            AbstractSyntaxTreePtr index = lhs->down->down;
            if (index == nullptr || index->right == nullptr || index->right->right != nullptr) {
                return false;
            }
            auto indexSlot = usableLocalSlot(index);
            if (!indexSlot.has_value()) {
                return false;
            }
            uint8_t column = 0;
            if (!positiveUInt8IndexLiteral(index->right, column)) {
                return false;
            }
            uint16_t valueIndex = 0;
            if (!constantValueIndex(rhs, valueIndex)) {
                return false;
            }
            emit(OpCode::ASGN_BRACES_CONST_COL, column, baseSlot.index, indexSlot->index,
                valueIndex);
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalBraceLocalAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            if (lhs == nullptr || lhs->down == nullptr || lhs->down->opNum != OP_BRACES
                || lhs->down->right != nullptr || containsEndKeyword(lhs->down)) {
                return false;
            }
            AbstractSyntaxTreePtr index = lhs->down->down;
            if (index == nullptr) {
                return false;
            }
            auto indexSlot = usableLocalSlot(index);
            auto valueSlot = usableLocalSlot(rhs);
            if (!indexSlot.has_value() || !valueSlot.has_value()) {
                return false;
            }
            if (index->right == nullptr) {
                emit(OpCode::ASGN_BRACES_LOCAL, 0, baseSlot.index, indexSlot->index,
                    valueSlot->index);
            } else {
                if (index->right->right != nullptr) {
                    return false;
                }
                uint8_t column = 0;
                if (!positiveUInt8IndexLiteral(index->right, column)) {
                    return false;
                }
                emit(OpCode::ASGN_BRACES_LOCAL_COL, column, baseSlot.index, indexSlot->index,
                    valueSlot->index);
            }
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        localDotField(AbstractSyntaxTreePtr node, const std::string& baseName, std::string& field)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->type != id_node || node->text != baseName
                || node->down == nullptr || node->down->opNum != OP_DOT
                || node->down->right != nullptr || node->down->down == nullptr) {
                return false;
            }
            field = node->down->down->text;
            return !field.empty();
        }

        bool
        localNestedDotFields(AbstractSyntaxTreePtr node, const std::string& baseName,
            std::string& parentField, std::string& childField)
        {
            node = simpleSingleNode(node);
            if (node == nullptr || node->type != id_node || node->text != baseName
                || node->down == nullptr || node->down->opNum != OP_DOT
                || node->down->right == nullptr || node->down->right->opNum != OP_DOT
                || node->down->down == nullptr || node->down->right->down == nullptr
                || node->down->right->right != nullptr) {
                return false;
            }
            parentField = node->down->down->text;
            childField = node->down->right->down->text;
            return !parentField.empty() && !childField.empty();
        }

        bool
        doubleLiteral(AbstractSyntaxTreePtr node, double& value)
        {
            node = simpleSingleNode(node);
            if (node == nullptr
                || (node->type != const_int_node && node->type != const_double_node)) {
                return false;
            }
            value = textToDouble(node->text);
            return true;
        }

        bool
        tryCompileLocalDotLocalAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            if (lhs == nullptr || lhs->down == nullptr || lhs->down->opNum != OP_DOT
                || lhs->down->right != nullptr || lhs->down->down == nullptr) {
                return false;
            }
            const std::string field = lhs->down->down->text;
            if (field.empty()) {
                return false;
            }
            auto valueSlot = usableLocalSlot(rhs);
            if (!valueSlot.has_value()) {
                return false;
            }
            emit(OpCode::ASGN_DOT_LOCAL, 0, baseSlot.index, chunk_.constants.addName(field),
                valueSlot->index);
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        // In-place single-level field assignment (localvar.field = rhs or localvar.(expr) = rhs) on
        // a local slot. ASGN_DOT / ASGN_DOTDYN mutate frame.locals[slot] without the load/assign
        // stack round-trip, so the base stays uniquely owned and building a struct field by field
        // is O(nFields), not O(nFields^2). Returns false for anything else so the caller falls back
        // to the generic path.
        bool
        compileLocalDotInPlaceAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            if (lhs == nullptr || lhs->down == nullptr || lhs->down->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr sub = lhs->down;
            if (sub->opNum == OP_DOT) {
                if (sub->down == nullptr || sub->down->text.empty()) {
                    return false;
                }
                compileExpr(rhs);
                emit(
                    OpCode::ASGN_DOT, 0, baseSlot.index, chunk_.constants.addName(sub->down->text));
                pop();
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
                }
                return true;
            }
            if (sub->opNum == OP_DOTDYN) {
                if (sub->down == nullptr) {
                    return false;
                }
                compileExpr(rhs);
                compileExpr(sub->down);
                emit(OpCode::ASGN_DOTDYN, 0, baseSlot.index);
                pop(2);
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
                }
                return true;
            }
            return false;
        }

        bool
        tryCompileLocalDotAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            if (lhs == nullptr || lhs->down == nullptr || lhs->down->opNum != OP_DOT
                || lhs->down->right != nullptr || lhs->down->down == nullptr) {
                return false;
            }
            const std::string field = lhs->down->down->text;
            if (field.empty()) {
                return false;
            }

            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            std::string sourceField;
            double increment = 0;
            if (!localDotField(expr->down, lhs->text, sourceField) || sourceField != field
                || !doubleLiteral(expr->down->right, increment)) {
                return false;
            }
            emit(OpCode::ASGN_DOT_PLUS_CONST, 0, baseSlot.index, chunk_.constants.addName(field),
                chunk_.constants.addDouble(increment));
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalNestedDotLocalAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            std::string parentField;
            std::string childField;
            if (!localNestedDotFields(lhs, lhs->text, parentField, childField)) {
                return false;
            }
            auto valueSlot = usableLocalSlot(rhs);
            if (!valueSlot.has_value()) {
                return false;
            }
            if (valueSlot->index > 254) {
                return false;
            }
            emit(OpCode::ASGN_DOT_DOT_LOCAL, static_cast<uint8_t>(valueSlot->index + 1),
                baseSlot.index, chunk_.constants.addName(parentField),
                chunk_.constants.addName(childField));
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
        }

        bool
        tryCompileLocalNestedDotAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& baseSlot, bool printIt)
        {
            std::string parentField;
            std::string childField;
            if (!localNestedDotFields(lhs, lhs->text, parentField, childField)) {
                return false;
            }

            AbstractSyntaxTreePtr expr = simpleSingleNode(rhs);
            if (expr == nullptr || expr->type != non_terminal || expr->opNum != OP_PLUS
                || expr->down == nullptr || expr->down->right == nullptr
                || expr->down->right->right != nullptr) {
                return false;
            }
            std::string sourceParentField;
            std::string sourceChildField;
            double increment = 0;
            if (!localNestedDotFields(expr->down, lhs->text, sourceParentField, sourceChildField)
                || sourceParentField != parentField || sourceChildField != childField
                || !doubleLiteral(expr->down->right, increment) || increment < 1 || increment > 255
                || increment != std::floor(increment)) {
                return false;
            }
            emit(OpCode::ASGN_DOT_DOT_PLUS_SMALL, static_cast<uint8_t>(increment), baseSlot.index,
                chunk_.constants.addName(parentField), chunk_.constants.addName(childField));
            slots_.markAssigned(lhs->text);
            if (printIt) {
                emit(OpCode::DISPLAY, 0, baseSlot.index, nameIndex(lhs->text));
            }
            return true;
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
                // the dedicated opcodes seed the slot from the calling workspace before mutating in
                // place, so a script's first indexed assignment can grow an inherited variable
                // without a copy through the stack
                if (slot.has_value() && slot->kind != SlotKind::Global
                    && (lhs->down->opNum == OP_PARENS || lhs->down->opNum == OP_BRACES)) {
                    compileExpr(rhs);
                    uint16_t nargs = 0;
                    compileAssignmentIndexArgs(lhs->down->down, slot->index, nargs);
                    emit(lhs->down->opNum == OP_PARENS ? OpCode::ASGN_PARENS : OpCode::ASGN_BRACES,
                        0, slot->index, nargs);
                    pop(static_cast<uint16_t>(nargs + 1));
                    slots_.markAssigned(lhs->text);
                    if (printIt) {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                    }
                    return;
                }
                // The dedicated field opcodes seed the slot from the calling workspace too, so a
                // field assignment to an inherited local mutates in place.
                if (slot.has_value() && slot->kind != SlotKind::Global
                    && compileLocalDotInPlaceAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (slot.has_value() && slot->kind != SlotKind::Global) {
                    emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                } else {
                    emit(OpCode::LOAD_GLOBAL,
                        loadContextFlags(lhs->text, INST_FLAG_ALLOW_UNDEFINED),
                        nameIndex(lhs->text));
                }
                push();
                compileExpr(rhs);
                compileStackValueAssign(lhs->down);
                if (slot.has_value() && slot->kind != SlotKind::Global) {
                    emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                    slots_.markAssigned(lhs->text);
                } else {
                    emit(OpCode::STORE_GLOBAL, storeContextFlags(lhs->text), nameIndex(lhs->text));
                }
                pop();
                if (printIt) {
                    if (slot.has_value() && slot->kind != SlotKind::Global) {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                    } else {
                        emit(
                            OpCode::LOAD_GLOBAL, loadContextFlags(lhs->text), nameIndex(lhs->text));
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
                if (tryCompileLocalNestedDotAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                if (tryCompileLocalNestedDotLocalAssign(lhs, rhs, *slot, printIt)) {
                    return;
                }
                compileChainedIndexedAssign(lhs, *slot, printIt);
                return;
            }
            if (tryCompileConstStringIndexedAssign(lhs, rhs, *slot, printIt)) {
                slots_.markAssigned(lhs->text);
                return;
            }
            if (tryCompileLocalDotLocalAssign(lhs, rhs, *slot, printIt)) {
                return;
            }
            if (tryCompileLocalDotAssign(lhs, rhs, *slot, printIt)) {
                return;
            }
            if (tryCompileLocalBraceConstAssign(lhs, rhs, *slot, printIt)) {
                return;
            }
            if (tryCompileLocalBraceLocalAssign(lhs, rhs, *slot, printIt)) {
                return;
            }
            if (lhs->down->opNum == OP_PARENS || lhs->down->opNum == OP_BRACES) {
                compileExpr(rhs);
                uint16_t nargs = 0;
                compileAssignmentIndexArgs(lhs->down->down, slot->index, nargs);
                emit(lhs->down->opNum == OP_PARENS ? OpCode::ASGN_PARENS : OpCode::ASGN_BRACES, 0,
                    slot->index, nargs);
                pop(static_cast<uint16_t>(nargs + 1));
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
                return;
            }
            // Single-level field assignment to a local struct mutates the slot in place. A
            // load/store round-trip copies the base, which makes adding fields repeatedly
            // O(nFields^2).
            if (compileLocalDotInPlaceAssign(lhs, rhs, *slot, printIt)) {
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

        // With valueTempSlot non-null, the right-hand side is loaded from that local slot instead
        // of lhs->right. The multi-output path uses this to route a stacked call result through the
        // chained-assignment code.
        void
        emitChainedAssignValue(AbstractSyntaxTreePtr lhs, const uint16_t* valueTempSlot)
        {
            if (valueTempSlot != nullptr) {
                emit(OpCode::LOAD_LOCAL, 0, *valueTempSlot);
                push();
                return;
            }
            compileExpr(lhs->right);
        }

        void
        compileChainedIndexedAssign(AbstractSyntaxTreePtr lhs, const SlotInfo& slot, bool printIt,
            bool loadContextBase = false, const uint16_t* valueTempSlot = nullptr)
        {
            compileChainedIndexedAssign(
                lhs, std::optional<SlotInfo>(slot), printIt, loadContextBase, valueTempSlot);
        }

        void
        compileChainedIndexedAssign(AbstractSyntaxTreePtr lhs, const std::optional<SlotInfo>& slot,
            bool printIt, bool loadContextBase = false, const uint16_t* valueTempSlot = nullptr)
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

            // the dedicated indexed-field opcode also creates the array from an
            // undefined base (and seeds the slot from the calling workspace),
            // and must handle growth loops whose first textual assignment is
            // the indexed field itself: keep it ahead of the never-assigned
            // stack fallback below
            if (slot.has_value() && slot->kind != SlotKind::Global && parents.size() == 1
                && parents[0]->opNum == OP_PARENS && sub->opNum == OP_DOT && sub->down != nullptr
                && countPeers(parents[0]->down) == 1) {
                emitChainedAssignValue(lhs, valueTempSlot);
                uint16_t nargs = 0;
                compileAssignmentIndexArgs(parents[0]->down, slot->index, nargs);
                emit(OpCode::ASGN_PARENS_DOT, 0, slot->index, nargs,
                    chunk_.constants.addName(sub->down->text));
                pop(static_cast<uint16_t>(nargs + 1));
                slots_.markAssigned(lhs->text);
                if (printIt) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                }
                return;
            }

            if (slot.has_value() && !loadContextBase && !slot->everAssigned && parents.size() == 1
                && (parents[0]->opNum == OP_PARENS || parents[0]->opNum == OP_BRACES)
                && (sub->opNum == OP_DOT || sub->opNum == OP_DOTDYN)) {
                emit(OpCode::LOAD_EMPTY);
                push();
                emit(OpCode::LOAD_EMPTY);
                push();
                emitChainedAssignValue(lhs, valueTempSlot);
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

            bool useLocalSlot = slot.has_value() && slot->kind != SlotKind::Global;
            if (useLocalSlot) {
                emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
            } else {
                emit(OpCode::LOAD_GLOBAL, loadContextFlags(lhs->text, INST_FLAG_ALLOW_UNDEFINED),
                    nameIndex(lhs->text));
            }
            push();
            // x.name(indices) = value, x.(expr)(indices) = value and their
            // braces forms: a table target receives both subscripts without
            // x.name being read; any other target runs the unchanged per-level
            // assignment below.
            const bool guardsTable
                = isDotParentFinalIndex(parents, sub, true) && !containsEndKeyword(sub->down);
            uint32_t tableJumpEnd = 0;
            if (guardsTable) {
                const uint16_t entryDepth = stackDepth_;
                uint32_t tableGuard = emit(OpCode::JUMP_IF_NOT_TABLE);
                emitChainedAssignValue(lhs, valueTempSlot);
                compileStackDotChainValueAssign(parents[0], sub, false);
                tableJumpEnd = emit(OpCode::JUMP);
                chunk_.patchJump(tableGuard, static_cast<uint16_t>(chunk_.code.size()));
                stackDepth_ = entryDepth;
            }
            for (AbstractSyntaxTreePtr parent : parents) {
                emit(OpCode::DUP);
                push();
                uint8_t parentFlags = INST_FLAG_ALLOW_UNDEFINED;
                if (parent->opNum == OP_PARENS) {
                    parentFlags |= INST_FLAG_INDEX_ONLY;
                }
                compileSubindex(parent, parentFlags);
            }

            emitChainedAssignValue(lhs, valueTempSlot);
            if (isDotParentFinalIndexUsingEnd(parents, sub)) {
                compileStackDotChainValueAssign(parents[0], sub);
            } else {
                compileStackValueAssign(sub);

                for (auto it = parents.rbegin(); it != parents.rend(); ++it) {
                    compileStackValueAssign(*it);
                }
            }
            if (guardsTable) {
                chunk_.patchJump(tableJumpEnd, static_cast<uint16_t>(chunk_.code.size()));
            }

            if (useLocalSlot) {
                emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
            } else {
                emit(OpCode::STORE_GLOBAL,
                    storeContextFlags(lhs->text, printIt ? INST_FLAG_PRINT : 0),
                    nameIndex(lhs->text));
            }
            pop();
            if (useLocalSlot) {
                slots_.markAssigned(lhs->text);
            }
            if (printIt) {
                if (useLocalSlot) {
                    emit(OpCode::DISPLAY, 0, slot->index, nameIndex(lhs->text));
                } else {
                    emit(OpCode::LOAD_GLOBAL, loadContextFlags(lhs->text), nameIndex(lhs->text));
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

        // With hasParentValue, the value of the field (read for `end`) sits
        // below the value and the indices. A dynamic field (never with
        // hasParentValue) pushes the field name or number above the value.
        void
        compileStackDotChainValueAssign(
            AbstractSyntaxTreePtr parent, AbstractSyntaxTreePtr sub, bool hasParentValue = true)
        {
            const bool isDynamic = parent != nullptr && parent->opNum == OP_DOTDYN;
            if (parent == nullptr || (parent->opNum != OP_DOT && !isDynamic)
                || (isDynamic && hasParentValue) || parent->down == nullptr || sub == nullptr
                || (sub->opNum != OP_PARENS && sub->opNum != OP_BRACES)) {
                unsupported(sub, "dot chain assignment");
            }
            if (isDynamic) {
                compileExpr(parent->down);
            }
            uint16_t nargs = 0;
            AbstractSyntaxTreePtr arg = sub->down;
            uint16_t total = countPeers(arg);
            while (arg != nullptr) {
                compileIndexExpr(arg, static_cast<uint16_t>(nargs + 1), nargs, total);
                ++nargs;
                arg = arg->right;
            }
            uint8_t flags = sub->opNum == OP_BRACES ? INST_FLAG_INDEX_ONLY : 0;
            if (isDynamic) {
                emit(OpCode::ASGN_DOTDYN_CHAIN_VALUE, flags, 0, nargs);
                pop(static_cast<uint16_t>(nargs + 2));
                return;
            }
            if (!hasParentValue) {
                flags |= INST_FLAG_NO_PARENT_VALUE;
            }
            emit(OpCode::ASGN_DOT_CHAIN_VALUE, flags, chunk_.constants.addName(parent->down->text),
                nargs);
            pop(static_cast<uint16_t>(hasParentValue ? nargs + 2 : nargs + 1));
        }

        // Index arguments of the in-place assignment opcodes never see their
        // base on the stack, so `end` is resolved against the local slot via
        // PUSH_END_CTX_LOCAL; the context is scoped to the argument so nested
        // indexed reads keep their own innermost `end`.
        void
        compileAssignmentIndexArgs(
            AbstractSyntaxTreePtr firstArg, uint16_t slotIndex, uint16_t& nargs)
        {
            uint16_t total = countPeers(firstArg);
            AbstractSyntaxTreePtr arg = firstArg;
            while (arg != nullptr) {
                if (indexArgContainsEnd(arg)) {
                    emit(OpCode::PUSH_END_CTX_LOCAL, 0, slotIndex, nargs, total);
                    compileAssignmentIndexExpr(arg);
                    emit(OpCode::POP_END_CTX);
                } else {
                    compileAssignmentIndexExpr(arg);
                }
                ++nargs;
                arg = arg->right;
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
                uint16_t baseSlot = 0;
                uint16_t indexSlot = 0;
                if (localBraceReadSlots(expr, baseSlot, indexSlot)) {
                    // brace extraction of a local through a local index keeps
                    // comma-list semantics via INST_FLAG_LIST but skips the
                    // generic LOAD/END_CTX/SUBIDX_BRACES sequence
                    emit(OpCode::LOAD_BRACES_LOCAL, INST_FLAG_LIST, baseSlot, indexSlot);
                    push();
                    return;
                }
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
                    if (node->text == "global") {
                        declaredGlobalNames_.insert(arg->text);
                    } else {
                        declaredPersistentNames_.insert(arg->text);
                    }
                    arg = arg->right;
                }
                return;
            }
            if (node->text == "import" && node->right != nullptr) {
                AbstractSyntaxTreePtr arg = node->right;
                while (arg != nullptr) {
                    emit(OpCode::DECLARE_IMPORT, 0, nameIndex(arg->text));
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
                        && arg->text != "functions" && arg->text != "classes"
                        && arg->text != "import") {
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
                    || arg->text == "functions" || arg->text == "classes"
                    || arg->text == "import") {
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

        // Assigns a multi-output result (already on the stack) to a multi-level indexed target such
        // as `s(idx).field`. The value goes to a temporary local and the chained-assignment path is
        // reused. Display is left to the caller.
        void
        compileMultiCallChainedTarget(
            AbstractSyntaxTreePtr target, const std::optional<SlotInfo>& slot)
        {
            uint16_t valueTempSlot = slots_.allocTempSlot();
            emit(OpCode::STORE_LOCAL, 0, valueTempSlot);
            pop();
            bool useContextBase = !slot.has_value() || slot->kind == SlotKind::Global
                || (chunk_.isScript && !slot->everAssigned);
            if (useContextBase) {
                if (slot.has_value() && slot->kind != SlotKind::Global) {
                    compileChainedIndexedAssign(target, *slot, false, true, &valueTempSlot);
                } else {
                    compileChainedIndexedAssign(target, std::nullopt, false, false, &valueTempSlot);
                }
                return;
            }
            compileChainedIndexedAssign(target, *slot, false, false, &valueTempSlot);
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
                    Error(L"Nelson:interpreter:syntaxError", L"Syntax error.");
                }
                if (lhs->type != non_terminal || lhs->opNum != OP_RHS || lhs->down == nullptr
                    || lhs->down->type != id_node) {
                    Error(L"Nelson:interpreter:syntaxError", L"Syntax error.");
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
            uint16_t nout = static_cast<uint16_t>(lhsNodes.size());
            // Only a bare `[c{...}] = rhs` is a brace comma-list target; a
            // trailing chain such as `[c{1}.field] = rhs` is a single indexed
            // target handled by the per-target chained-assignment path below.
            bool singleBraceCommaListTarget = lhsNodes.size() == 1 && lhsNodes[0]->down != nullptr
                && lhsNodes[0]->down->opNum == OP_BRACES && lhsNodes[0]->down->right == nullptr;
            // `[s.field] = rhs` with a struct array `s` expands to numel(s) targets, so the RHS
            // needs that many outputs (unknown at compile time). The bare `s.field` / `s.(expr)`
            // form uses the dynamic-output path of `[c{:}] = rhs`.
            bool singleDotCommaListTarget = lhsNodes.size() == 1 && lhsNodes[0]->down != nullptr
                && (lhsNodes[0]->down->opNum == OP_DOT || lhsNodes[0]->down->opNum == OP_DOTDYN)
                && lhsNodes[0]->down->down != nullptr && lhsNodes[0]->down->right == nullptr;
            bool singleCommaListTarget = singleBraceCommaListTarget || singleDotCommaListTarget;
            // Set once the RHS is invoked with a runtime-computed output count. Only then is the
            // comma-list target filled from the reserved dynamic-output slots.
            bool dynamicCommaListAssign = false;
            constexpr uint16_t DYNAMIC_OUTPUT_STACK_RESERVE = 16;
            bool indexedCommaListSource = false;
            if (callee->down != nullptr) {
                AbstractSyntaxTreePtr lastSubindex = lastNonNullSubindex(callee->down);
                auto calleeSlot = slots_.resolve(callee->text);
                bool calleeIsKnownVariable = calleeSlot.has_value()
                    && calleeSlot->kind != SlotKind::Global && calleeSlot->everAssigned;
                indexedCommaListSource = lastSubindex != nullptr
                    && (lastSubindex->opNum == OP_BRACES || lastSubindex->opNum == OP_DOT
                        || lastSubindex->opNum == OP_DOTDYN
                        || (calleeIsKnownVariable && chainEndsWithDotCall(callee->down)));
            }
            // `[x{1:N}] = c{:}` / `[s.f] = c{:}`: a single comma-list target fed from a comma-list
            // RHS. The target count is known only at runtime, so the RHS is expanded to that many
            // values (see the branch below).
            bool commaListToCommaListTarget = indexedCommaListSource && singleCommaListTarget;
            // Runtime output-count spec for a multi-subscript brace target such as `[c{1:2,1}]`:
            // load the base, evaluate each subscript (so `end` and `:` resolve against it), then
            // multiply the element counts with LOAD_CONTENT_TARGET_COUNT. Leaves the spec on the
            // stack.
            auto emitMultiBraceCountSpec = [&](AbstractSyntaxTreePtr targetNode) {
                auto targetSlot = slots_.resolve(targetNode->text);
                if (!targetSlot.has_value() || targetSlot->kind == SlotKind::Global) {
                    emit(OpCode::LOAD_GLOBAL,
                        loadContextFlags(targetNode->text, INST_FLAG_ALLOW_UNDEFINED),
                        nameIndex(targetNode->text));
                } else {
                    emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, targetSlot->index);
                }
                push();
                uint16_t nargs = 0;
                AbstractSyntaxTreePtr arg = targetNode->down->down;
                uint16_t total = countPeers(arg);
                while (arg != nullptr) {
                    compileIndexExpr(arg, nargs, nargs, total);
                    ++nargs;
                    arg = arg->right;
                }
                emit(OpCode::LOAD_CONTENT_TARGET_COUNT, 0, 0, nargs, 0);
                pop(static_cast<uint16_t>(nargs + 1));
                push();
            };
            auto emitCommaListTargetCountSpec = [&]() {
                if (singleBraceCommaListTarget) {
                    AbstractSyntaxTreePtr outputCountExpr = lhsNodes[0]->down->down;
                    if (outputCountExpr == nullptr) {
                        unsupported(lhsRoot, "dynamic multi-output brace lhs");
                    }
                    if (outputCountExpr->right != nullptr) {
                        emitMultiBraceCountSpec(lhsNodes[0]);
                        return;
                    }
                    bool allTargetElements = outputCountExpr != nullptr
                        && outputCountExpr->type == non_terminal
                        && outputCountExpr->opNum == OP_ALL;
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
                } else {
                    auto targetSlot = slots_.resolve(lhsNodes[0]->text);
                    if (!targetSlot.has_value() || targetSlot->kind == SlotKind::Global) {
                        emit(OpCode::LOAD_FIELD_TARGET_COUNT, INST_FLAG_GLOBAL,
                            nameIndex(lhsNodes[0]->text));
                    } else {
                        emit(OpCode::LOAD_FIELD_TARGET_COUNT, 0, targetSlot->index);
                    }
                    push();
                }
            };
            if (commaListToCommaListTarget) {
                // Produce the RHS as a comma-list sentinel, expand it to the runtime number of
                // targets (error if it yields fewer values), then distribute the values with the
                // multi-value assignment tail of the function-call form.
                compileNamedCallOrIndex(callee, 1, false, true);
                emitCommaListTargetCountSpec();
                emit(OpCode::EXPAND_COMMA_LIST_DYNAMIC, 0, 0);
                pop(2);
                for (uint16_t k = 0; k < DYNAMIC_OUTPUT_STACK_RESERVE; ++k) {
                    push();
                }
                dynamicCommaListAssign = true;
            } else if (indexedCommaListSource) {
                compileNamedCallOrIndex(callee, nout, false, true);
                emit(OpCode::EXPAND_COMMA_LIST, 0, nout);
                pop();
                for (uint16_t k = 0; k < nout; ++k) {
                    push();
                }
            } else if (callee->down != nullptr && callee->down->opNum != OP_PARENS) {
                compileNamedCallOrIndex(callee, nout);
                for (uint16_t k = 1; k < nout; ++k) {
                    push();
                }
            } else {
                auto calleeSlot = slots_.resolve(callee->text);
                bool calleeIsLocal = calleeSlot.has_value() && calleeSlot->kind != SlotKind::Global
                    && calleeSlot->everAssigned;
                if (calleeIsLocal) {
                    emit(OpCode::LOAD_LOCAL, 0, calleeSlot->index);
                    push();
                }
                uint16_t nargs = 0;
                if (callee->down != nullptr) {
                    if (callee->down->opNum != OP_PARENS || hasNonNullRight(callee->down)) {
                        unsupported(callee, "multi-output callee indexing");
                    }
                    AbstractSyntaxTreePtr arg = callee->down->down;
                    while (arg != nullptr) {
                        compileCallArgument(arg);
                        ++nargs;
                        arg = arg->right;
                    }
                }

                if (singleCommaListTarget) {
                    if (singleBraceCommaListTarget && lhsNodes[0]->down->down != nullptr
                        && lhsNodes[0]->down->down->right != nullptr) {
                        emitMultiBraceCountSpec(lhsNodes[0]);
                    } else if (singleBraceCommaListTarget) {
                        AbstractSyntaxTreePtr outputCountExpr = lhsNodes[0]->down->down;
                        if (outputCountExpr == nullptr) {
                            unsupported(lhsRoot, "dynamic multi-output brace lhs");
                        }
                        bool allTargetElements = outputCountExpr != nullptr
                            && outputCountExpr->type == non_terminal
                            && outputCountExpr->opNum == OP_ALL;
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
                    } else {
                        // singleDotCommaListTarget: the RHS output count is
                        // max(1, numel(target)), read at runtime from the struct
                        // array bound to the target variable (an undefined or
                        // empty target still requests one output to grow a 1x1).
                        auto targetSlot = slots_.resolve(lhsNodes[0]->text);
                        if (!targetSlot.has_value() || targetSlot->kind == SlotKind::Global) {
                            emit(OpCode::LOAD_FIELD_TARGET_COUNT, INST_FLAG_GLOBAL,
                                nameIndex(lhsNodes[0]->text));
                        } else {
                            emit(OpCode::LOAD_FIELD_TARGET_COUNT, 0, targetSlot->index);
                        }
                        push();
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
                    dynamicCommaListAssign = true;
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
            }
            if (dynamicCommaListAssign) {
                AbstractSyntaxTreePtr target = lhsNodes[0];
                auto slot = slots_.resolve(target->text);
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::LOAD_GLOBAL,
                        loadContextFlags(target->text, INST_FLAG_ALLOW_UNDEFINED),
                        nameIndex(target->text));
                } else {
                    emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                }
                push();
                if (singleBraceCommaListTarget) {
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
                } else if (target->down->opNum == OP_DOT) {
                    emit(OpCode::ASGN_DOT_MULTI_VALUE, 0,
                        chunk_.constants.addName(target->down->down->text), 0);
                    pop(DYNAMIC_OUTPUT_STACK_RESERVE);
                } else {
                    compileExpr(target->down->down);
                    push();
                    emit(OpCode::ASGN_DOTDYN_MULTI_VALUE, 0, 0, 0);
                    pop(static_cast<uint16_t>(DYNAMIC_OUTPUT_STACK_RESERVE + 1));
                }
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::STORE_GLOBAL, storeContextFlags(target->text),
                        nameIndex(target->text));
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
                if (target->down != nullptr && target->down->right != nullptr) {
                    // A multi-level indexed target such as `s(idx).field` needs more than one stack
                    // opcode: stash the value in a temporary and reuse the chained-assignment path.
                    // Display is deferred to the trailing block, so printIt=false.
                    compileMultiCallChainedTarget(target, slot);
                    continue;
                }
                if (target->down != nullptr) {
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::LOAD_GLOBAL,
                            loadContextFlags(target->text, INST_FLAG_ALLOW_UNDEFINED),
                            nameIndex(target->text));
                    } else {
                        emit(OpCode::LOAD_LOCAL, INST_FLAG_ALLOW_UNDEFINED, slot->index);
                    }
                    push();
                    emit(OpCode::SWAP);
                    compileStackValueAssign(target->down);
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::STORE_GLOBAL, storeContextFlags(target->text),
                            nameIndex(target->text));
                    } else {
                        emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                        slots_.markAssigned(target->text);
                    }
                    pop();
                    continue;
                }
                if (!slot.has_value() || slot->kind == SlotKind::Global) {
                    emit(OpCode::STORE_GLOBAL, storeContextFlags(target->text),
                        nameIndex(target->text));
                } else {
                    emit(OpCode::ASSIGN, printIt ? INST_FLAG_PRINT : 0, slot->index);
                    slots_.markAssigned(target->text);
                }
                pop();
            }
            if (printIt) {
                for (AbstractSyntaxTreePtr target : lhsNodes) {
                    if (isPlaceholderIdentifier(target->text)) {
                        continue;
                    }
                    auto slot = slots_.resolve(target->text);
                    if (!slot.has_value() || slot->kind == SlotKind::Global) {
                        emit(OpCode::LOAD_GLOBAL, loadContextFlags(target->text),
                            nameIndex(target->text));
                        push();
                        emit(OpCode::DISPLAY_STACK, 0, nameIndex(target->text));
                        emit(OpCode::POP);
                        pop();
                    } else {
                        emit(OpCode::DISPLAY, 0, slot->index, nameIndex(target->text));
                    }
                }
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
                    chunk_.constants.addArray(ArrayOf::uint8Scalar(textToUint8(node->text))));
                push();
                return;
            case const_int8_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int8Scalar(textToInt8(node->text))));
                push();
                return;
            case const_uint16_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::uint16Scalar(textToUint16(node->text))));
                push();
                return;
            case const_int16_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int16Scalar(textToInt16(node->text))));
                push();
                return;
            case const_uint32_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::uint32Scalar(textToUint32(node->text))));
                push();
                return;
            case const_int32_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int32Scalar(textToInt32(node->text))));
                push();
                return;
            case const_uint64_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::uint64Scalar(textToUint64(node->text))));
                push();
                return;
            case const_int64_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(ArrayOf::int64Scalar(textToInt64(node->text))));
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
                        decomplexify(ArrayOf::singleComplexScalar(0, textToSingle(node->text)))));
                push();
                return;
            case const_dcomplex_node:
                emit(OpCode::LOAD_CONST, 0,
                    chunk_.constants.addArray(
                        decomplexify(ArrayOf::dsingleComplexScalar(0, textToDouble(node->text)))));
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
                // B = 1 + local slot: a loop runs this read again after the
                // first assignment of the local, which only the slot holds
                uint16_t localOperand = (slot.has_value() && slot->kind != SlotKind::Global
                                            && slot->index < UINT16_MAX)
                    ? static_cast<uint16_t>(slot->index + 1)
                    : 0;
                emit(OpCode::LOAD_GLOBAL, loadContextFlags(node->text), nameIndex(node->text),
                    localOperand);
            } else {
                emit(OpCode::LOAD_LOCAL, 0, slot->index);
            }
            push();
        }

        // Compiles obj@Super(args) / method@Super(args). Returns the number of values left on the
        // stack (1 for the constructor form, the updated object; requestedNout for the method
        // form).
        int
        compileSuperclassCall(AbstractSyntaxTreePtr node, int requestedNout)
        {
            // node: OP_SUPERCLASS_CALL, down = left operand (id_node: the object
            // in a constructor, or the method name in a method), left->right =
            // the super reference (id_node 'Super' with a PARENS child for args,
            // possibly preceded by DOT children for a qualified name).
            AbstractSyntaxTreePtr left = node->down;
            AbstractSyntaxTreePtr superRef = (left != nullptr) ? left->right : nullptr;
            if (left == nullptr || left->type != id_node || superRef == nullptr
                || superRef->type != id_node) {
                unsupported(node, "superclass call");
                return 0;
            }
            std::string superName = superRef->text;
            AbstractSyntaxTreePtr parens = nullptr;
            for (AbstractSyntaxTreePtr child = superRef->down; child != nullptr;
                child = child->right) {
                if (child->opNum == OP_DOT && child->down != nullptr) {
                    superName += "." + child->down->text;
                } else if (child->opNum == OP_PARENS) {
                    parens = child;
                }
            }
            // Constructor form when the '@' operand is a real local variable
            // (the constructed object); otherwise it is a method name.
            auto slot = slots_.resolve(left->text);
            const bool isConstructorForm
                = slot.has_value() && slot->kind != SlotKind::Global && slot->everAssigned;
            // The constructor form always needs the returned (updated) object.
            const uint16_t callNout = isConstructorForm ? 1 : static_cast<uint16_t>(requestedNout);
            uint16_t nargs = 2;
            if (isConstructorForm) {
                compileVarLoad(left);
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(superName));
                push();
            } else {
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(superName));
                push();
                emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(left->text));
                push();
            }
            for (AbstractSyntaxTreePtr arg = (parens != nullptr) ? parens->down : nullptr;
                arg != nullptr; arg = arg->right) {
                compileCallArgument(arg);
                ++nargs;
            }
            // Native superclass call: a dedicated opcode dispatched in C++
            // (evaluateClassdefSuperCall).
            emit(OpCode::SUPER_CALL, isConstructorForm ? INST_FLAG_SUPER_CONSTRUCTOR : 0, 0, nargs,
                callNout);
            pop(nargs);
            for (uint16_t i = 0; i < callNout; ++i) {
                push();
            }
            if (isConstructorForm) {
                // A super constructor call updates the object in place, even when
                // written bare (obj@Super(...)) with no explicit assignment. Store
                // the result back into the object slot, keeping a copy on the stack
                // as the expression value.
                emit(OpCode::DUP);
                push();
                emit(OpCode::ASSIGN, 0, slot->index);
                slots_.markAssigned(left->text);
                pop();
                return 1;
            }
            return callNout;
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
            case OP_SUPERCLASS_CALL:
                compileSuperclassCall(node, 1);
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
            case OP_UMINUS: {
                // Fold unary minus over an integer literal by converting the negated magnitude text
                // directly, so the signed minimum stays representable: -9223372036854775808i64 must
                // yield intmin('int64'), which negating the saturated positive magnitude at run
                // time cannot give.
                AbstractSyntaxTreePtr operand = node->down;
                if (operand != nullptr) {
                    switch (operand->type) {
                    case const_int8_node:
                        emit(OpCode::LOAD_CONST, 0,
                            chunk_.constants.addArray(
                                ArrayOf::int8Scalar(textToInt8("-" + operand->text))));
                        push();
                        return;
                    case const_int16_node:
                        emit(OpCode::LOAD_CONST, 0,
                            chunk_.constants.addArray(
                                ArrayOf::int16Scalar(textToInt16("-" + operand->text))));
                        push();
                        return;
                    case const_int32_node:
                        emit(OpCode::LOAD_CONST, 0,
                            chunk_.constants.addArray(
                                ArrayOf::int32Scalar(textToInt32("-" + operand->text))));
                        push();
                        return;
                    case const_int64_node:
                        emit(OpCode::LOAD_CONST, 0,
                            chunk_.constants.addArray(
                                ArrayOf::int64Scalar(textToInt64("-" + operand->text))));
                        push();
                        return;
                    default:
                        break;
                    }
                }
                compileExpr(node->down);
                emit(unaryOpcode(node->opNum));
                return;
            }
            case OP_UPLUS:
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
                if (tryCompileTableDynamicDotIndex(sub)) {
                    continue;
                }
                uint8_t flags = 0;
                if (sub->opNum == OP_PARENS && sub->right != nullptr) {
                    flags |= INST_FLAG_INDEX_ONLY;
                }
                if (sub->opNum == OP_DOT && sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && !dotParensIndexUsesEnd(sub)) {
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
            // the body as written, for func2str and display (the content is re-parsed)
            uint16_t definitionIdx = chunk_.constants.addName(node->text);
            emit(OpCode::MAKE_FH_ANONYMOUS, 0, contentIdx, argsIdx, definitionIdx);
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
                // an indexed or returned value has no input name, even when a
                // builtin stored or returned a named argument
                emit(OpCode::SET_NAME, 0, nameIndex(""));
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

        static AbstractSyntaxTreePtr
        unwrapSingleRhs(AbstractSyntaxTreePtr node)
        {
            if (node != nullptr && node->type == non_terminal && node->opNum == OP_RHS
                && node->down != nullptr && node->right == nullptr) {
                return node->down;
            }
            return node;
        }

        bool
        tryCompileStringLiteralConstructor(
            AbstractSyntaxTreePtr ident, int nout, bool outputOptional, bool commaList)
        {
            if (ident == nullptr || ident->text != "string" || nout != 1 || outputOptional
                || commaList || ident->down == nullptr || ident->down->opNum != OP_PARENS
                || ident->down->right != nullptr) {
                return false;
            }
            AbstractSyntaxTreePtr arg = unwrapSingleRhs(ident->down->down);
            if (arg == nullptr || arg->right != nullptr
                || (arg->type != const_character_array_node && arg->type != const_string_node)) {
                return false;
            }
            emit(OpCode::LOAD_CONST, 0, chunk_.constants.addStringObj(arg->text));
            push();
            return true;
        }

        // A bare decimal integer literal is lexed into a const_double_node whose text is the exact
        // digit string (a floating literal keeps a '.' or an exponent). Reject an empty string.
        static bool
        isPlainDecimalIntegerText(const std::string& text)
        {
            if (text.empty()) {
                return false;
            }
            for (const char c : text) {
                if (c < '0' || c > '9') {
                    return false;
                }
            }
            return true;
        }

        // int64(9007199254740993) applied directly to a bare integer literal converts the literal
        // text straight to the target type, instead of rounding to double first. This keeps
        // integers in (2^53, 2^64] exact, like the typed suffix literals. Only a plain decimal
        // integer literal qualifies; a floating literal, variable or expression keeps the
        // double-then-cast behavior. A leading unary minus is folded so the signed minimum stays
        // representable. A matrix or cell element is compiled as a comma-list source, so this is
        // reached with commaList set. An integer scalar is exactly one value, so a single constant
        // is correct there too.
        bool
        tryCompileIntegerLiteralConstructor(AbstractSyntaxTreePtr ident, int nout)
        {
            if (ident == nullptr || nout != 1 || ident->down == nullptr
                || ident->down->opNum != OP_PARENS || ident->down->right != nullptr) {
                return false;
            }
            NODE_TYPE target = null_node;
            if (ident->text == "int8") {
                target = const_int8_node;
            } else if (ident->text == "int16") {
                target = const_int16_node;
            } else if (ident->text == "int32") {
                target = const_int32_node;
            } else if (ident->text == "int64") {
                target = const_int64_node;
            } else if (ident->text == "uint8") {
                target = const_uint8_node;
            } else if (ident->text == "uint16") {
                target = const_uint16_node;
            } else if (ident->text == "uint32") {
                target = const_uint32_node;
            } else if (ident->text == "uint64") {
                target = const_uint64_node;
            } else {
                return false;
            }
            AbstractSyntaxTreePtr arg = unwrapSingleRhs(ident->down->down);
            if (arg == nullptr || arg->right != nullptr) {
                return false;
            }
            bool negative = false;
            if (arg->type == non_terminal && arg->opNum == OP_UMINUS && arg->down != nullptr
                && arg->down->right == nullptr && arg->down->type == const_double_node) {
                negative = true;
                arg = arg->down;
            }
            if (arg->type != const_double_node || !isPlainDecimalIntegerText(arg->text)) {
                return false;
            }
            const bool isUnsigned = (target == const_uint8_node || target == const_uint16_node
                || target == const_uint32_node || target == const_uint64_node);
            // A negative literal always saturates an unsigned type to zero; leave
            // that to the ordinary conversion so the text converter is never fed a
            // leading minus (which the unsigned parse would wrap around instead).
            if (negative && isUnsigned) {
                return false;
            }
            const std::string text = negative ? ("-" + arg->text) : arg->text;
            ArrayOf value;
            switch (target) {
            case const_int8_node:
                value = ArrayOf::int8Scalar(textToInt8(text));
                break;
            case const_int16_node:
                value = ArrayOf::int16Scalar(textToInt16(text));
                break;
            case const_int32_node:
                value = ArrayOf::int32Scalar(textToInt32(text));
                break;
            case const_int64_node:
                value = ArrayOf::int64Scalar(textToInt64(text));
                break;
            case const_uint8_node:
                value = ArrayOf::uint8Scalar(textToUint8(text));
                break;
            case const_uint16_node:
                value = ArrayOf::uint16Scalar(textToUint16(text));
                break;
            case const_uint32_node:
                value = ArrayOf::uint32Scalar(textToUint32(text));
                break;
            default:
                value = ArrayOf::uint64Scalar(textToUint64(text));
                break;
            }
            emit(OpCode::LOAD_CONST, 0, chunk_.constants.addArray(value));
            push();
            return true;
        }

        bool
        positiveUInt16IndexLiteral(AbstractSyntaxTreePtr index, uint16_t& ordinal)
        {
            double indexValue = textToDouble(index->text);
            if (!std::isfinite(indexValue) || indexValue <= 0.
                || indexValue != std::floor(indexValue)
                || indexValue > static_cast<double>(std::numeric_limits<uint16_t>::max())) {
                return false;
            }
            ordinal = static_cast<uint16_t>(indexValue);
            return true;
        }

        bool
        tryCompileConstStringIndexedAssign(AbstractSyntaxTreePtr lhs, AbstractSyntaxTreePtr rhs,
            const SlotInfo& slot, bool printIt)
        {
            if (printIt || lhs == nullptr || lhs->down == nullptr || lhs->down->opNum != OP_PARENS
                || lhs->down->right != nullptr || containsEndKeyword(lhs->down)) {
                return false;
            }
            AbstractSyntaxTreePtr index = unwrapSingleRhs(lhs->down->down);
            if (index == nullptr || index->right != nullptr
                || (index->type != const_int_node && index->type != const_double_node)) {
                return false;
            }
            AbstractSyntaxTreePtr value = unwrapSingleRhs(rhs);
            if (value == nullptr || value->right != nullptr
                || (value->type != const_string_node
                    && value->type != const_character_array_node)) {
                return false;
            }
            uint16_t ordinal = 0;
            if (positiveUInt16IndexLiteral(index, ordinal)) {
                uint8_t flags = value->type == const_string_node ? INST_FLAG_VALUE_IS_STRING : 0;
                uint16_t valueConstant = chunk_.constants.addString(value->text);
                emit(OpCode::ASGN_STRING_SCALAR_CONST, flags, slot.index, ordinal, valueConstant);
                return true;
            }
            uint16_t indexConstant = chunk_.constants.addDouble(textToDouble(index->text));
            uint16_t valueConstant = value->type == const_string_node
                ? chunk_.constants.addStringObj(value->text)
                : chunk_.constants.addString(value->text);
            emit(OpCode::ASGN_PARENS_CONST, 0, slot.index, indexConstant, valueConstant);
            return true;
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
        compileSubindexChain(AbstractSyntaxTreePtr sub, int nout, bool outputOptional,
            bool commaList, bool firstSubindex)
        {
            while (sub != nullptr) {
                if (tryCompileTableDynamicDotIndex(sub, nout)) {
                    firstSubindex = false;
                    continue;
                }
                uint8_t flags = 0;
                bool dotParensSubindex = sub->opNum == OP_DOT && sub->right != nullptr
                    && sub->right->opNum == OP_PARENS;
                // The output count applies only to the last operation of the chain. An intermediate
                // call keeps its one output so the next subindex has a value: `c.m(a).m(b)` would
                // otherwise call m(a) with nargout 0 and lose the returned object.
                AbstractSyntaxTreePtr chainContinuation
                    = dotParensSubindex ? sub->right->right : sub->right;
                if (firstSubindex && chainContinuation == nullptr
                    && (sub->opNum == OP_PARENS || sub->opNum == OP_BRACES || dotParensSubindex)) {
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
                if (sub->opNum == OP_DOT && sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && !dotParensIndexUsesEnd(sub)) {
                    if (commaList && sub->right->right == nullptr) {
                        flags |= INST_FLAG_LIST;
                    }
                    uint16_t nargs = 0;
                    AbstractSyntaxTreePtr arg = sub->right->down;
                    while (arg != nullptr) {
                        compileIndexExpr(arg, nargs, nargs, countPeers(sub->right->down));
                        ++nargs;
                        arg = arg->right;
                    }
                    uint16_t requestedOutputs
                        = (flags & INST_FLAG_LIST) != 0 ? static_cast<uint16_t>(nout) : 1;
                    emit(OpCode::SUBIDX_DOT, flags, nargs,
                        chunk_.constants.addName(sub->down->text), requestedOutputs);
                    pop(nargs);
                    firstSubindex = false;
                    sub = sub->right->right;
                    continue;
                }
                uint16_t extra = 0;
                if ((flags & INST_FLAG_LIST) != 0) {
                    extra = static_cast<uint16_t>(nout);
                } else if (sub->opNum == OP_PARENS && sub->right == nullptr && nout > 1) {
                    extra = static_cast<uint16_t>(nout);
                }
                compileSubindex(sub, flags, extra);
                firstSubindex = false;
                sub = sub->right;
            }
        }

        void
        compileResolvedCallAndTail(OpCode op, uint16_t calleeIndex, AbstractSyntaxTreePtr callSub,
            int nout, bool outputOptional, bool commaList, uint8_t extraFlags = 0)
        {
            if (callSub == nullptr || callSub->opNum != OP_PARENS) {
                unsupported(callSub, "resolved call");
            }
            const bool hasTail = hasNonNullRight(callSub);
            uint16_t nargs = 0;
            AbstractSyntaxTreePtr arg = callSub->down;
            while (arg != nullptr) {
                compileCallArgument(arg);
                ++nargs;
                arg = arg->right;
            }
            uint16_t requestedOutputs
                = hasTail ? static_cast<uint16_t>(1) : static_cast<uint16_t>(nout);
            uint8_t flags = extraFlags;
            if (!hasTail && outputOptional) {
                flags |= INST_FLAG_PRINT;
            }
            emit(op, flags, calleeIndex, nargs, requestedOutputs);
            pop(nargs);
            if (requestedOutputs > 0) {
                push();
            }
            if (hasTail) {
                compileSubindexChain(callSub->right, nout, outputOptional, commaList, false);
            }
        }

        // Unknown dotted calls retain variable/head-function fallback. A class-like
        // target uses member resolution, not raw function-table dispatch: class
        // registration can expose private static names after this code was compiled.
        // Confirmed package functions keep their direct named-call path.
        bool
        tryCompilePackageFunctionGuard(AbstractSyntaxTreePtr ident, AbstractSyntaxTreePtr sub,
            int nout, bool outputOptional, bool commaList, bool preferDotted = false)
        {
            if (ident->text == "true" || ident->text == "false") {
                return false;
            }
            std::string dottedName = ident->text;
            AbstractSyntaxTreePtr current = sub;
            AbstractSyntaxTreePtr callSub = nullptr;
            while (current != nullptr) {
                if (current->opNum != OP_DOT || current->down == nullptr
                    || current->down->type != id_node) {
                    return false;
                }
                dottedName += "." + current->down->text;
                if (current->right != nullptr && current->right->opNum == OP_PARENS) {
                    callSub = current->right;
                    break;
                }
                current = current->right;
            }
            // `end` or `:` inside the argument list only makes sense with the
            // variable interpretation; keep the plain chain lowering there.
            if (callSub == nullptr || containsAllIndex(callSub->down)
                || containsDirectEndKeyword(callSub->down)) {
                return false;
            }
            const uint16_t entryDepth = stackDepth_;
            uint32_t guard = emit(OpCode::LOAD_GLOBAL_OR_JUMP,
                loadContextFlags(ident->text, preferDotted ? INST_FLAG_PREFER_DOTTED : 0), 0,
                nameIndex(ident->text), preferDotted ? nameIndex(dottedName) : 0);
            push();
            compileSubindexChain(sub, nout, outputOptional, commaList, true);
            uint32_t jumpEnd = emit(OpCode::JUMP);
            const uint16_t chainExitDepth = stackDepth_;
            chunk_.patchJump(guard, static_cast<uint16_t>(chunk_.code.size()));
            stackDepth_ = entryDepth;
            if (!preferDotted) {
                emit(OpCode::ENSURE_DOTTED_FUNCTION, 0, nameIndex(dottedName),
                    nameIndex(ident->text));
            }
            compileResolvedCallAndTail(
                preferDotted ? OpCode::CALL_CLASSDEF_MEMBER : OpCode::CALL_NAMED,
                preferDotted ? chunk_.constants.addName(dottedName) : nameIndex(dottedName),
                callSub, nout, outputOptional, commaList, preferDotted ? INST_FLAG_INDEX_ONLY : 0);
            chunk_.patchJump(jumpEnd, static_cast<uint16_t>(chunk_.code.size()));
            stackDepth_ = std::max(chainExitDepth, stackDepth_);
            return true;
        }

        // A `Name.member(args)` reference whose class cannot be confirmed at compile time still
        // dispatches through the runtime CALL_CLASSDEF_MEMBER handler in a script or immediate
        // (`-e`) chunk. There the search path is often extended earlier in the same chunk (e.g.
        // `addpath` before the call), so a lowercase classdef on that path is not loadable yet, and
        // the uppercase hint in looksLikeRuntimeClassdefReference misses it. The handler resolves a
        // same-named variable, a classdef static member, a package function, then a base function
        // returning an object, so deferring is safe for other heads. Function bodies are compiled
        // lazily, after the path is set, so they keep the compile-time check and the static-method
        // lowering.
        bool
        deferClassdefMemberToRuntime(const std::string& className)
        {
            return chunk_.isScript || looksLikeRuntimeClassdefReference(className);
        }

        // A bare dotted reference with no parentheses, e.g. `qq.rr.helper` (a nested package
        // function). It resolves like the call `pkg.fn()`, so a pure `id.id...id` chain naming a
        // package function (or runtime classdef member) must not load the head as a variable and
        // report an undefined variable. The classdef and package branches above fire only for a
        // trailing call or a class that loads at compile time, so a bare multi-level package
        // function reaches this fallback. A variable head still wins.
        bool
        tryCompileBareDottedPackageReference(
            AbstractSyntaxTreePtr ident, AbstractSyntaxTreePtr sub, int nout, bool outputOptional)
        {
            if (nout > 1 || ident->text == "true" || ident->text == "false") {
                return false;
            }
            std::string dottedName = ident->text;
            AbstractSyntaxTreePtr current = sub;
            while (current != nullptr) {
                // Only a pure `id.id...id` chain qualifies: a parenthesised or
                // braced continuation is a call/index handled by the guard and
                // classdef-member lowerings above, and a dynamic field keeps its
                // ordinary variable/field lowering.
                if (current->opNum != OP_DOT || current->down == nullptr
                    || current->down->type != id_node) {
                    return false;
                }
                if (current->right != nullptr
                    && (current->right->opNum == OP_PARENS || current->right->opNum == OP_BRACES
                        || current->right->opNum == OP_DOTDYN)) {
                    return false;
                }
                dottedName += "." + current->down->text;
                current = current->right;
            }
            const uint8_t printFlag = outputOptional ? INST_FLAG_PRINT : 0;
            // A package function already on the path (e.g. a function body compiled
            // after its module was loaded) is a direct dotted call with no args.
            if (isKnownDottedFunction(dottedName)) {
                emit(OpCode::CALL_NAMED, printFlag, nameIndex(dottedName), 0,
                    static_cast<uint16_t>(nout));
                if (nout > 0) {
                    push();
                }
                return true;
            }
            // Otherwise defer to the runtime member handler, as for the parenthesised form, so a
            // package added to the path later in the chunk still works.
            if (deferClassdefMemberToRuntime(ident->text)) {
                emit(OpCode::CALL_CLASSDEF_MEMBER, printFlag, chunk_.constants.addName(dottedName),
                    0, static_cast<uint16_t>(nout));
                if (nout > 0) {
                    push();
                }
                return true;
            }
            return false;
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
            bool definitelyGlobal = slot.has_value() && slot->kind == SlotKind::Global;
            // `end` in a subscript forces the variable-index path only when the
            // name is a known variable (declared global/persistent). For an
            // unknown name (a function call) `end` inside an argument expression
            // refers to the enclosing index context, so the direct-only check
            // keeps it a call, e.g. A(round(end/2)) must still call round.
            bool nameIsDeclaredVariable = definitelyGlobal || isDeclaredGlobalName(ident->text)
                || isDeclaredPersistentName(ident->text);
            bool endForcesVariableIndex = nameIsDeclaredVariable
                ? containsEndKeywordThisLevel(sub->down)
                : containsDirectEndKeyword(sub->down);
            if (definitelyLocal && sub->opNum == OP_DOT && sub->down != nullptr
                && sub->down->type == id_node && !commaList && nout <= 1
                && (sub->right == nullptr || sub->right->opNum == OP_DOT
                    || sub->right->opNum == OP_DOTDYN)) {
                emit(OpCode::LOAD_LOCAL_DOT, 0, slot->index,
                    chunk_.constants.addName(sub->down->text));
                push();
                if (sub->right != nullptr) {
                    compileSubindexChain(sub->right, nout, outputOptional, commaList, false);
                }
                return;
            }
            if (definitelyLocal && sub->opNum == OP_PARENS && !commaList && !outputOptional
                && nout <= 1 && sub->down != nullptr && sub->down->right == nullptr
                && !containsEndKeyword(sub)) {
                uint16_t indexSlot = 0;
                uint16_t offsetSlot = 0;
                double offset = 0.;
                if (localSlotWithConstantOffset(sub->down, indexSlot, offset)) {
                    const bool collectChain
                        = sub->right != nullptr && nout <= 1 && chainIsCollectCapable(sub);
                    if (collectChain) {
                        emit(OpCode::CHAIN_BEGIN, 0, slot->index, 0);
                    }
                    if (offset == 0.) {
                        emit(OpCode::LOAD_PARENS_LOCAL, 0, slot->index, indexSlot);
                    } else {
                        emit(OpCode::LOAD_PARENS_LOCAL_OFFSET, 0, slot->index, indexSlot,
                            chunk_.constants.addDouble(offset));
                    }
                    push();
                    if (sub->right != nullptr) {
                        compileSubindexChain(sub->right, nout, outputOptional, commaList, false);
                    }
                    if (collectChain) {
                        emit(OpCode::CHAIN_END);
                    }
                    return;
                }
                if (localSlotMinusLocalOffset(sub->down, indexSlot, offsetSlot)) {
                    const bool collectChain
                        = sub->right != nullptr && nout <= 1 && chainIsCollectCapable(sub);
                    if (collectChain) {
                        emit(OpCode::CHAIN_BEGIN, 0, slot->index, 0);
                    }
                    emit(OpCode::LOAD_PARENS_LOCAL_MINUS_LOCAL, 0, slot->index, indexSlot,
                        offsetSlot);
                    push();
                    if (sub->right != nullptr) {
                        compileSubindexChain(sub->right, nout, outputOptional, commaList, false);
                    }
                    if (collectChain) {
                        emit(OpCode::CHAIN_END);
                    }
                    return;
                }
            }
            if (definitelyLocal && sub->opNum == OP_BRACES && !commaList && nout <= 1
                && sub->down != nullptr && sub->down->right == nullptr
                && !containsEndKeyword(sub)) {
                auto indexSlot = usableLocalSlot(sub->down);
                if (indexSlot.has_value()) {
                    emit(OpCode::LOAD_BRACES_LOCAL, 0, slot->index, indexSlot->index);
                    push();
                    if (sub->right != nullptr) {
                        compileSubindexChain(sub->right, nout, outputOptional, commaList, false);
                    }
                    return;
                }
            }
            if (!definitelyLocal && !definitelyGlobal
                && tryCompileStringLiteralConstructor(ident, nout, outputOptional, commaList)) {
                return;
            }
            if (!definitelyLocal && !definitelyGlobal
                && tryCompileIntegerLiteralConstructor(ident, nout)) {
                return;
            }
            bool explicitArgumentCountCall
                = (ident->text == "nargin" || ident->text == "nargout") && sub->opNum == OP_PARENS;
            if (sub->opNum == OP_PARENS && sub->right == nullptr
                && (!definitelyLocal || explicitArgumentCountCall) && !containsAllIndex(sub->down)
                && (!endForcesVariableIndex || allowsDirectEndAsFunctionArgument(ident->text))) {
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

            // A captured variable of an anonymous function has no local slot (it
            // is loaded from the closure), so it is not "definitelyLocal"; still,
            // it is a variable, so member access on it (capturedVar.field(...))
            // must not be mistaken for a classdef reference.
            const bool identIsCaptured = !chunk_.capturedNames.empty()
                && std::find(chunk_.capturedNames.begin(), chunk_.capturedNames.end(), ident->text)
                    != chunk_.capturedNames.end();
            bool firstSubindex = true;
            if (sub->opNum == OP_PARENS && sub->right != nullptr && !definitelyLocal
                && !containsAllIndex(sub->down)
                && (!endForcesVariableIndex || allowsDirectEndAsFunctionArgument(ident->text))) {
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
            } else if (!definitelyLocal && !identIsCaptured && sub->opNum == OP_DOT
                && sub->down != nullptr && sub->down->type == id_node) {
                const std::string memberName = sub->down->text;
                // static empty-array constructor for builtin types:
                //   TypeName.empty            -> 0x0 empty array of that class
                //   TypeName.empty(sz, ...)   -> requested-size empty array
                // Lowered to the `__empty__` helper with the class name as the
                // first argument. Only the bare and simple parenthesized forms
                // are intercepted; anything with a trailing chain falls through.
                if (memberName == "empty" && isBuiltinEmptyTypeName(ident->text)) {
                    AbstractSyntaxTreePtr parens = sub->right;
                    const bool bareForm = !hasNonNullRight(sub);
                    const bool parensForm = parens != nullptr && parens->opNum == OP_PARENS
                        && !hasNonNullRight(parens) && !containsAllIndex(parens->down)
                        && !containsDirectEndKeyword(parens->down);
                    if (bareForm || parensForm) {
                        emit(OpCode::LOAD_CONST, 0, chunk_.constants.addString(ident->text));
                        push();
                        uint16_t nargs = 1;
                        if (parensForm) {
                            AbstractSyntaxTreePtr arg = parens->down;
                            while (arg != nullptr) {
                                compileCallArgument(arg);
                                ++nargs;
                                arg = arg->right;
                            }
                        }
                        emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                            nameIndex("__empty__"), nargs, static_cast<uint16_t>(nout));
                        pop(nargs);
                        if (nout > 0) {
                            push();
                        }
                        return;
                    }
                }
                std::string classdefFunctionName;
                const std::string directClassName = classdefReferencePrefix(ident->text, sub);
                if (!directClassName.empty()) {
                    AbstractSyntaxTreePtr classSub = subindexForQualifiedName(sub, directClassName);
                    if (classSub != nullptr && classSub->right != nullptr
                        && classSub->right->opNum == OP_PARENS) {
                        compileResolvedCallAndTail(OpCode::CALL_NAMED, nameIndex(directClassName),
                            classSub->right, nout, outputOptional, commaList);
                        return;
                    }
                    if (classSub != nullptr && classSub->right != nullptr
                        && classSub->right->opNum == OP_DOT && classSub->right->down != nullptr
                        && classSub->right->down->type == id_node) {
                        const std::string directMemberName = classSub->right->down->text;
                        if (classSub->right->right != nullptr
                            && classSub->right->right->opNum == OP_PARENS
                            && classdefResolveStaticMethodFunction(directClassName,
                                directMemberName, classdefFunctionName,
                                chunk_.classdefAccessClassName)) {
                            compileResolvedCallAndTail(OpCode::CALL_NAMED,
                                nameIndex(classdefFunctionName), classSub->right->right, nout,
                                outputOptional, commaList);
                            return;
                        }
                        if (classSub->right->right != nullptr
                            && classSub->right->right->opNum == OP_PARENS
                            && looksLikeRuntimeClassdefReference(directClassName)) {
                            compileResolvedCallAndTail(OpCode::CALL_CLASSDEF_MEMBER,
                                chunk_.constants.addName(directClassName + "." + directMemberName),
                                classSub->right->right, nout, outputOptional, commaList,
                                INST_FLAG_INDEX_ONLY);
                            return;
                        }
                        if (!hasNonNullRight(classSub->right) && nout > 0) {
                            bool resolvedDirectClassMember
                                = classdefResolveConstantPropertyFunction(directClassName,
                                      directMemberName, classdefFunctionName,
                                      chunk_.classdefAccessClassName)
                                || classdefResolveEnumerationMemberFunction(
                                    directClassName, directMemberName, classdefFunctionName);
                            if (resolvedDirectClassMember) {
                                emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                                    nameIndex(classdefFunctionName), 0,
                                    static_cast<uint16_t>(nout));
                                push();
                                return;
                            }
                            if (looksLikeRuntimeClassdefReference(directClassName)) {
                                emit(OpCode::CALL_CLASSDEF_MEMBER,
                                    outputOptional ? INST_FLAG_PRINT : 0,
                                    chunk_.constants.addName(
                                        directClassName + "." + directMemberName),
                                    0, static_cast<uint16_t>(nout));
                                push();
                                return;
                            }
                        }
                    }
                }
                const std::string packageClassName = ident->text + "." + memberName;
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && classdefLoadClass(packageClassName)) {
                    compileResolvedCallAndTail(OpCode::CALL_NAMED, nameIndex(packageClassName),
                        sub->right, nout, outputOptional, commaList);
                    return;
                }
                if (sub->right != nullptr && sub->right->opNum == OP_DOT
                    && sub->right->down != nullptr && sub->right->down->type == id_node) {
                    const std::string packageMemberName = sub->right->down->text;
                    if (sub->right->right != nullptr && sub->right->right->opNum == OP_PARENS
                        && classdefResolveStaticMethodFunction(packageClassName, packageMemberName,
                            classdefFunctionName, chunk_.classdefAccessClassName)) {
                        compileResolvedCallAndTail(OpCode::CALL_NAMED,
                            nameIndex(classdefFunctionName), sub->right->right, nout,
                            outputOptional, commaList);
                        return;
                    }
                    if (sub->right->right != nullptr && sub->right->right->opNum == OP_PARENS
                        && looksLikeRuntimeClassdefReference(packageClassName)) {
                        compileResolvedCallAndTail(OpCode::CALL_CLASSDEF_MEMBER,
                            chunk_.constants.addName(packageClassName + "." + packageMemberName),
                            sub->right->right, nout, outputOptional, commaList,
                            INST_FLAG_INDEX_ONLY);
                        return;
                    }
                    if (!hasNonNullRight(sub->right) && nout > 0) {
                        bool resolvedPackageClassMember
                            = classdefResolveConstantPropertyFunction(packageClassName,
                                  packageMemberName, classdefFunctionName,
                                  chunk_.classdefAccessClassName)
                            || classdefResolveEnumerationMemberFunction(
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
                    && classdefResolveStaticMethodFunction(ident->text, memberName,
                        classdefFunctionName, chunk_.classdefAccessClassName)) {
                    compileResolvedCallAndTail(OpCode::CALL_NAMED, nameIndex(classdefFunctionName),
                        sub->right, nout, outputOptional, commaList);
                    return;
                }
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && isKnownDottedFunction(ident->text + "." + memberName)) {
                    // A single-level package function (+Pkg/func.m) already on the path is lowered
                    // to a direct CALL_NAMED, like nested package calls, avoiding the run-time
                    // CALL_CLASSDEF_MEMBER fallback. Names not yet resolvable (package added later)
                    // still take that fallback.
                    compileResolvedCallAndTail(OpCode::CALL_NAMED,
                        nameIndex(ident->text + "." + memberName), sub->right, nout, outputOptional,
                        commaList);
                    return;
                }
                if (sub->right != nullptr && sub->right->opNum == OP_PARENS
                    && !containsAllIndex(sub->right->down)
                    && !containsDirectEndKeyword(sub->right->down)
                    && deferClassdefMemberToRuntime(ident->text)) {
                    // A whole-array colon `head.member(:)` or an `end` inside the parentheses is
                    // field indexing, never a member call: CALL_CLASSDEF_MEMBER would evaluate the
                    // colon/end as a call argument (OP_ALL is unsupported). Leave these to the
                    // variable + subindex chain path below. The CALL_NAMED lowerings above have the
                    // same guards.
                    compileResolvedCallAndTail(OpCode::CALL_CLASSDEF_MEMBER,
                        chunk_.constants.addName(ident->text + "." + memberName), sub->right, nout,
                        outputOptional, commaList, INST_FLAG_INDEX_ONLY);
                    return;
                }
                if (!hasNonNullRight(sub) && nout > 0) {
                    bool resolvedClassMember
                        = classdefResolveConstantPropertyFunction(ident->text, memberName,
                              classdefFunctionName, chunk_.classdefAccessClassName)
                        || classdefResolveEnumerationMemberFunction(
                            ident->text, memberName, classdefFunctionName);
                    if (resolvedClassMember) {
                        emit(OpCode::CALL_NAMED, outputOptional ? INST_FLAG_PRINT : 0,
                            nameIndex(classdefFunctionName), 0, static_cast<uint16_t>(nout));
                        push();
                        return;
                    }
                    if (deferClassdefMemberToRuntime(ident->text)) {
                        emit(OpCode::CALL_CLASSDEF_MEMBER, outputOptional ? INST_FLAG_PRINT : 0,
                            chunk_.constants.addName(ident->text + "." + memberName), 0,
                            static_cast<uint16_t>(nout));
                        push();
                        return;
                    }
                }
                std::string dottedName;
                AbstractSyntaxTreePtr dottedCallSub = nullptr;
                // A whole-array colon `head.Member(:)` or an `end` inside the parentheses is field
                // indexing, not a dotted call, even when an uppercase segment looks class-like.
                // Lower it as a variable + subindex chain (see the guard above).
                if (collectDottedCall(ident, dottedName, dottedCallSub)
                    && !containsAllIndex(dottedCallSub->down)
                    && !containsDirectEndKeyword(dottedCallSub->down)) {
                    // A dotted name that is only class-like (uppercase segment)
                    // and not resolvable when compiled may also be a member chain
                    // on the object returned by the head function, e.g.
                    // `py.collections.OrderedDict(args)`: the guarded form keeps
                    // the dotted function first and falls back to that chain.
                    if (!isKnownDottedFunction(dottedName)
                        && tryCompilePackageFunctionGuard(
                            ident, sub, nout, outputOptional, commaList, true)) {
                        return;
                    }
                    compileResolvedCallAndTail(OpCode::CALL_NAMED, nameIndex(dottedName),
                        dottedCallSub, nout, outputOptional, commaList);
                    return;
                }
                if (tryCompilePackageFunctionGuard(ident, sub, nout, outputOptional, commaList)) {
                    return;
                }
                if (tryCompileBareDottedPackageReference(ident, sub, nout, outputOptional)) {
                    return;
                }
                compileVarLoad(ident);
            } else {
                compileVarLoad(ident);
            }
            const bool collectChain = slot.has_value() && slot->kind != SlotKind::Global
                && slot->everAssigned && nout <= 1 && firstSubindex && chainIsCollectCapable(sub);
            if (collectChain) {
                const uint16_t firstOpKind = sub->opNum == OP_BRACES ? 1
                    : sub->opNum == OP_DOT                           ? 2
                                                                     : 0;
                emit(OpCode::CHAIN_BEGIN, 0, slot->index, firstOpKind);
            }
            compileSubindexChain(sub, nout, outputOptional, commaList, firstSubindex);
            if (collectChain) {
                emit(OpCode::CHAIN_END);
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

        static bool
        hasClassLikeDottedSegment(const std::string& dottedName)
        {
            size_t start = dottedName.find('.');
            while (start != std::string::npos && start + 1 < dottedName.size()) {
                size_t segmentStart = start + 1;
                size_t segmentEnd = dottedName.find('.', segmentStart);
                std::string segment = dottedName.substr(segmentStart,
                    segmentEnd == std::string::npos ? std::string::npos
                                                    : segmentEnd - segmentStart);
                if (!segment.empty()
                    && std::isupper(static_cast<unsigned char>(segment.front())) != 0) {
                    return true;
                }
                start = segmentEnd;
            }
            return false;
        }

        static bool
        isKnownDottedFunction(const std::string& dottedName)
        {
            if (dottedName.find('.') == std::string::npos) {
                return false;
            }
            FunctionDefPtr builtinFunction;
            if (BuiltInFunctionDefManager::getInstance()->find(dottedName, builtinFunction)) {
                return true;
            }
            std::wstring filename;
            return PathFunctionIndexerManager::getInstance()->find(dottedName, filename);
        }

        static bool
        collectDottedCall(
            AbstractSyntaxTreePtr ident, std::string& dottedName, AbstractSyntaxTreePtr& callSub)
        {
            if (ident == nullptr || ident->type != id_node || ident->down == nullptr) {
                return false;
            }
            dottedName = ident->text;
            AbstractSyntaxTreePtr current = ident->down;
            while (current != nullptr) {
                if (current->opNum != OP_DOT || current->down == nullptr
                    || current->down->type != id_node) {
                    return false;
                }
                dottedName += "." + current->down->text;
                if (current->right != nullptr && current->right->opNum == OP_PARENS) {
                    callSub = current->right;
                    return hasClassLikeDottedSegment(dottedName)
                        || isKnownDottedFunction(dottedName);
                }
                current = current->right;
            }
            return false;
        }

        // x.(expr)(indices) read: a table, timetable or eventtable value gets
        // both subscripts (SUBIDX_DOTDYN_CHAIN: a timetable variable can be
        // indexed by row times); any other value runs the per-level reads.
        bool
        tryCompileTableDynamicDotIndex(AbstractSyntaxTreePtr& sub, int nout = 1)
        {
            if (sub == nullptr || sub->opNum != OP_DOTDYN || sub->down == nullptr
                || sub->right == nullptr || sub->right->opNum != OP_PARENS
                || containsEndKeyword(sub->right->down)) {
                return false;
            }
            AbstractSyntaxTreePtr parens = sub->right;
            const uint16_t entryDepth = stackDepth_;
            uint32_t tableGuard = emit(OpCode::JUMP_IF_NOT_TABLE);
            compileExpr(sub->down);
            uint16_t nargs = 0;
            uint16_t total = countPeers(parens->down);
            for (AbstractSyntaxTreePtr arg = parens->down; arg != nullptr; arg = arg->right) {
                compileIndexExpr(arg, static_cast<uint16_t>(nargs + 1), nargs, total);
                ++nargs;
            }
            emit(OpCode::SUBIDX_DOTDYN_CHAIN, 0, 0, nargs);
            pop(static_cast<uint16_t>(nargs + 1));
            uint32_t tableJumpEnd = emit(OpCode::JUMP);
            chunk_.patchJump(tableGuard, static_cast<uint16_t>(chunk_.code.size()));
            stackDepth_ = entryDepth;
            compileSubindex(sub);
            const bool isLast = parens->right == nullptr;
            compileSubindex(parens, isLast ? 0 : INST_FLAG_INDEX_ONLY,
                isLast && nout > 1 ? static_cast<uint16_t>(nout) : 0);
            chunk_.patchJump(tableJumpEnd, static_cast<uint16_t>(chunk_.code.size()));
            sub = parens->right;
            return true;
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
            {
                uint16_t baseSlot = 0;
                uint16_t indexSlot = 0;
                if (localBraceReadSlots(expr, baseSlot, indexSlot)) {
                    // brace extraction of a local through a local index cannot
                    // reference `end`, so the end context is skipped; the LIST
                    // flag keeps comma-list expansion at the consuming opcode
                    emit(OpCode::LOAD_BRACES_LOCAL, INST_FLAG_LIST, baseSlot, indexSlot);
                    push();
                    return;
                }
            }
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

        // `{string.empty}`, `{datetime.empty}`, `{containers.Map}`: a head that is
        // not a known variable but names a class is a static reference, not a
        // struct field comma-list.
        bool
        cellHeadNamesClass(const std::string& name, AbstractSyntaxTreePtr sub)
        {
            auto slot = slots_.resolve(name);
            if (slot.has_value() && (slot->kind == SlotKind::Global || slot->everAssigned)) {
                return false;
            }
            return isBuiltinEmptyTypeName(name) || classdefLoadClass(name)
                || !classdefReferencePrefix(name, sub).empty();
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

        // `pkg.fn(args)` as a statement is a call, not an expression being displayed: like other
        // bare calls it asks for no output, so a function whose effect happens when nargout is zero
        // really acts. Any returned value is still captured in `ans` and displayed by the print
        // flag of the emitted call. Only a chain compileNamedCallOrIndex resolves to a dotted call
        // qualifies; a field access, an indexed field, and an assigned or captured head stay on the
        // display path.
        bool
        isDottedFunctionCallStatement(AbstractSyntaxTreePtr ident)
        {
            if (ident == nullptr || ident->type != id_node || ident->down == nullptr) {
                return false;
            }
            // peek() rather than resolve(): a probe must not add the head name
            // to the slot tables.
            auto slot = slots_.peek(ident->text);
            if (slot.has_value() && slot->kind != SlotKind::Global && slot->everAssigned) {
                return false;
            }
            if (!chunk_.capturedNames.empty()
                && std::find(chunk_.capturedNames.begin(), chunk_.capturedNames.end(), ident->text)
                    != chunk_.capturedNames.end()) {
                return false;
            }
            std::string dottedName = ident->text;
            AbstractSyntaxTreePtr current = ident->down;
            while (current != nullptr) {
                if (current->opNum != OP_DOT || current->down == nullptr
                    || current->down->type != id_node) {
                    return false;
                }
                dottedName += "." + current->down->text;
                if (current->right != nullptr && current->right->opNum == OP_PARENS) {
                    // A tail after the call indexes its result, so the call
                    // still has to produce one; a `:` or an `end` among the
                    // arguments means the head is being indexed, not called.
                    return !hasNonNullRight(current->right)
                        && !containsAllIndex(current->right->down)
                        && !containsDirectEndKeyword(current->right->down)
                        && isKnownDottedFunction(dottedName);
                }
                current = current->right;
            }
            return false;
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
            if (ident->type != id_node || ident->down == nullptr
                || cellHeadNamesClass(ident->text, ident->down)) {
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
    compileInto(BytecodeChunk& chunk, AbstractSyntaxTreePtr body,
        const stringVector& inheritedGlobals = stringVector())
    {
        SlotMap slots(chunk.isScript);
        for (size_t k = 0; k < chunk.argNames.size(); ++k) {
            slots.addArg(chunk.argNames[k], static_cast<uint16_t>(k));
        }
        CompilerContext compiler(chunk, slots);
        compiler.seedInheritedGlobals(inheritedGlobals);
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
        fuseScalarExpressions(chunk);
        auto namesContain = [](const auto& list, const char* name) {
            return std::find(list.begin(), list.end(), name) != list.end();
        };
        chunk.usesNarginNargout = namesContain(chunk.names, "nargin")
            || namesContain(chunk.names, "nargout") || namesContain(chunk.localNames, "nargin")
            || namesContain(chunk.localNames, "nargout");
        chunk.directLeafOpsAllowed = std::all_of(chunk.code.begin(), chunk.code.end(),
            [](const Instruction& ins) { return isDirectLeafMacroOpcode(ins.op); });
    }
    //=============================================================================
} // namespace
//=============================================================================
std::unique_ptr<BytecodeChunk>
BytecodeCompiler::compileFunction(AbstractSyntaxTreePtr body, const std::string& functionName,
    const std::wstring& sourcePath, const stringVector& argNames, const stringVector& retNames,
    const std::string& classdefAccessClassName, const stringVector& inheritedGlobals)
{
    auto chunk = makeChunk(
        functionName, sourcePath, false, argNames, retNames, {}, classdefAccessClassName);
    compileInto(*chunk, body, inheritedGlobals);
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
    chunk->isAnonymous = true;
    compileInto(*chunk, body);
    return chunk;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
