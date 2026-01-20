//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "Evaluator.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "CheckIfWhileCondition.hpp"
#include "PredefinedErrorMessages.hpp"
#include "MException.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
#include "CallbackQueue.hpp"
#include "EventQueue.hpp"
#include <functional>
//=============================================================================
namespace Nelson {
//=============================================================================
//!
//@Module IF-ELSEIF-ELSE Conditional Statements
//@@Section FLOW
//@@Usage
// The @|if| and @|else| statements form a control structure for
// conditional execution.  The general syntax involves an @|if|
// test, followed by zero or more @|elseif| clauses, and finally
// an optional @|else| clause:
//@[
//  if conditional_expression_1
//    statements_1
//  elseif conditional_expression_2
//    statements_2
//  elseif conditional_expresiion_3
//    statements_3
//  ...
//  else
//    statements_N
//  end
//@]
// Note that a conditional expression is considered true if
// the real part of the result of the expression contains
// any non-zero elements (this strange convention is adopted
// for compatibility with MATLAB).
//@@Examples
// Here is an example of a function that uses an @|if| statement
//@{ if_test.m
// function c = if_test(a)
//  if (a == 1)
//     c = 'one';
//  elseif (a==2)
//     c = 'two';
//  elseif (a==3)
//     c = 'three';
//  else
//     c = 'something else';
//  end
//@}
// Some examples of @|if_test| in action:
//@<
// if_test(1)
// if_test(2)
// if_test(3)
// if_test(pi)
//@>
//!
void
Evaluator::ifStatement(AbstractSyntaxTreePtr t)
{
    struct CallStackGuard
    {
        CallStack& cs;
        bool active;
        explicit CallStackGuard(CallStack& c, size_t id) : cs(c), active(true) { cs.pushID(id); }
        ~CallStackGuard()
        {
            if (active) {
                cs.popID();
            }
        }
        CallStackGuard(const CallStackGuard&) = delete;
        CallStackGuard&
        operator=(const CallStackGuard&)
            = delete;
    } guard(callstack, static_cast<size_t>(t->getContext()));

    if (conditionedStatement(t)) {
        return;
    }

    AbstractSyntaxTreePtr nextBlock = t->right;
    if (nextBlock != nullptr && nextBlock->opNum == OP_ELSEIFBLOCK) {
        for (AbstractSyntaxTreePtr s = nextBlock->down; s != nullptr; s = s->right) {
            if (conditionedStatement(s)) {
                return;
            }
        }
        nextBlock = nextBlock->right;
    }

    if (nextBlock != nullptr) {
        block(nextBlock);
    }
}
//=============================================================================
//!
//@Module WHILE While Loop
//@@Section FLOW
//@@Usage
// The @|while| loop executes a set of statements as long as
// a the test condition remains @|true|.  The syntax of a
//@|while| loop is
//@[
//  while test_expression
//     statements
//  end
//@]
// Note that a conditional expression is considered true if
// the real part of the result of the expression contains
// any non-zero elements (this strange convention is adopted
// for compatibility with MATLAB).
//@@Examples
// Here is a @|while| loop that adds the integers from @|1|
// to @|100|:
//@<
// accum = 0;
// k=1;
// while (k<100), accum = accum + k; k = k + 1; end
// accum
//@>
//!
void
Evaluator::whileStatement(AbstractSyntaxTreePtr t)
{
    AbstractSyntaxTreePtr testCondition;
    ArrayOf condVar;
    AbstractSyntaxTreePtr codeBlock;
    bool conditionTrue;
    bool breakEncountered;
    // Use corrected line (while keyword) so dbstack/stepping point at the condition line
    size_t whileLine = getLinePosition(t);
    callstack.pushID(whileLine);

    // Helper to handle breakpoints at the while condition line (each evaluation pass)
    auto handleConditionBreakpoint = [&]() -> bool {
        if (onBreakpoint(t)) {
            bpActive = true;
            while (true) {
                debugCLI();

                if (state == NLS_STATE_ABORT) {
                    bpActive = false;
                    callstack.popID();
                    return true;
                }

                if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
                    bpActive = false;
                    resetDebugDepth();
                    callstack.popID();
                    return true;
                }

                if (state == NLS_STATE_DEBUG_QUIT_ALL) {
                    bpActive = false;
                    state = NLS_STATE_ABORT;
                    callstack.popID();
                    return true;
                }

                if (state == NLS_STATE_DEBUG_QUIT) {
                    callstack.popID();
                    state = NLS_STATE_ABORT;
                    return true;
                }
                if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_STEP) {
                    if (state == NLS_STATE_DEBUG_STEP) {
                        bpActive = true;
                    } else {
                        bpActive = false;
                    }
                    state = NLS_STATE_OK;
                    break;
                }
            }
        }
        return false;
    };

    testCondition = t;
    codeBlock = t->right;
    breakEncountered = false;
    // Breakpoint before first condition evaluation
    if (handleConditionBreakpoint()) {
        return;
    }

    condVar = expression(testCondition);
    conditionTrue = checkIfWhileCondition(condVar);
    context->enterLoop();
    while (conditionTrue && !breakEncountered) {
        block(codeBlock);
        if (state == NLS_STATE_RETURN || state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
            break;
        }
        if (state == NLS_STATE_CONTINUE) {
            resetState();
        }
        breakEncountered = (state == NLS_STATE_BREAK);
        if (!breakEncountered) {
            // Breakpoint on each subsequent condition evaluation
            if (handleConditionBreakpoint()) {
                return;
            }
            condVar = expression(testCondition);
            conditionTrue = checkIfWhileCondition(condVar);
        } else {
            resetState();
        }
    }
    context->exitLoop();
    callstack.popID();
}
//=============================================================================
//!
//@Module FOR For Loop
//@@Section FLOW
//@@Usage
// The @|for| loop executes a set of statements with an
// index variable looping through each element in a vector.
// The syntax of a @|for| loop is one of the following:
//@[
//  for (variable=expression)
//     statements
//  end
//@]
// Alternately, the parenthesis can be eliminated
//@[
//  for variable=expr
//     statements
//  end
//@]
// or alternately, the index variable can be pre-initialized
// with the vector of values it is going to take:
//@[
//  for variable
//     statements
//  end
//@]
// The third form is essentially equivalent to @|for variable=variable|,
// where @|variable| is both the index variable and the set of values
// over which the for loop executes.  See the examples section for
// an example of this form of the @|for| loop.
//@@Examples
// Here we write @|for| loops to add all the integers from
//@|1| to @|100|.  We will use all three forms of the @|for|
// statement.
//@<
// accum = 0;
// for (i=1:100); accum = accum + i; end
// accum
//@>
// The second form is functionally the same, without the
// extra parenthesis
//@<
// accum = 0;
// for i=1:100; accum = accum + i; end
// accum
//@>
// In the third example, we pre-initialize the loop variable
// with the values it is to take
//!
//=============================================================================
// Generic ForLoopHelper with stride support. Used by typed helpers below.
//
// Optimizations introduced:
//  - conservative AST check to detect whether the loop body can reassign the
//    index variable or contain function calls that might mutate workspace.
//  - when the body is "safe" the variable lookup/allocation is hoisted and the
//    internal ArrayOf pointer is cached across iterations (big win for tight loops).
//  - semantics preserved: if the body may reassign the index variable we fall
//    back to the original (safe) codepath.
//
// Note: this is a conservative optimization only; it does not attempt to
// analyze side-effects of called functions. A future patch can add an
// opt-in parallel path (TBB/OpenMP) once we have explicit guarantees about
// user code side-effects.
//=============================================================================
// Search an AST subtree for an identifier name. Conservative and fast.
static bool
findIdentifierInSubtree(AbstractSyntaxTreePtr node, const std::string& name)
{
    for (AbstractSyntaxTreePtr cur = node; cur != nullptr; cur = cur->right) {
        if (cur->type == id_node && cur->text == name) {
            return true;
        }
        if (cur->down && findIdentifierInSubtree(cur->down, name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Conservatively determine whether the loop body may reassign `varName` or
// contains function-call constructs (which may have side-effects). If true
// we must *not* cache the loop variable across iterations.
static bool
loopBodyMayModifyIndex(AbstractSyntaxTreePtr node, const std::string& varName)
{
    if (!node) {
        return false;
    }
    for (AbstractSyntaxTreePtr cur = node; cur != nullptr; cur = cur->right) {
        // Direct assignment: check only the LHS entries (do NOT inspect the RHS).
        // The LHS may be a list (a,b = ...), so iterate sibling LHS nodes but do
        // not descend into the assignment RHS (which is stored as the parent->right).
        if (cur->opNum == OP_ASSIGN) {
            for (AbstractSyntaxTreePtr lhs = cur->down; lhs != nullptr; lhs = lhs->right) {
                // search only within the LHS subtree (do not follow lhs->right beyond
                // the LHS list)
                if (lhs && findIdentifierInSubtree(lhs, varName)) {
                    return true;
                }
            }
        }
        // Conservative: function-call nodes may have side-effects (assignin/eval/etc.).
        // Note: OP_RHS is a normal expression/variable reference and must NOT be
        // treated as a side-effecting node — doing so produced false-positives
        // (e.g. `s = s + i;`).
        if (cur->opNum == OP_SCALL || cur->opNum == OP_MULTICALL) {
            return true;
        }
        // Recurse into children (but avoid re-inspecting the RHS of an assignment
        // as a sibling of the OP_ASSIGN node; that RHS will be visited via the
        // parent's right pointer in the caller loop). We still recurse into
        // expression children for other node types.
        if (cur->down && loopBodyMayModifyIndex(cur->down, varName)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
void
ForLoopHelper(AbstractSyntaxTreePtr codeBlock, NelsonType indexClass, const T* indexSet,
    indexType count, indexType stride, const std::string& indexName, Evaluator* eval)
{
    Scope* scope = eval->getContext()->getCurrentScope();
    if (scope->isLockedVariable(indexName)) {
        Error(_W("Redefining permanent variable."));
    }

    // If the loop body cannot possibly reassign the index variable (conservative
    // check), we can hoist the lookup/allocation and cache the ArrayOf* pointer
    // across iterations. This avoids a lookup per-iteration in the hot path.
    const bool bodyMayModifyIndex = loopBodyMayModifyIndex(codeBlock, indexName);

    // --- Reduction fast-path detector -------------------------------------------------
    // Detect very common pattern:
    //   <acc> = <acc> + <index>
    //   <acc> = <index> + <acc>
    //   <acc> = <acc> - <index>
    // Conservative requirements:
    //  - codeBlock contains a single statement which is a simple assignment
    //  - LHS is a single identifier (accumulator)
    //  - RHS contains only the accumulator identifier, the index identifier and
    //    numeric literals, and uses only + or - operators
    //  - no function calls, no other identifiers
    //  - stride == 1 (no complex interleaved data)
    //  - accumulator exists in scope, is scalar and is a supported numeric type
    // If all satisfied, perform a native accumulation and write back once.
    auto detectSimpleReduction = [&](AbstractSyntaxTreePtr block, const std::string& accName,
                                     const std::string& idxName) -> bool {
        if (!block || !block->down) {
            return false;
        }
        // single statement only
        if (block->down->right != nullptr) {
            return false;
        }
        AbstractSyntaxTreePtr stmt = block->down;
        if (stmt->opNum != OP_ASSIGN) {
            return false;
        }
        // LHS must be single identifier equal to accName
        AbstractSyntaxTreePtr lhs = stmt->down;
        if (!lhs || lhs->type != id_node) {
            return false;
        }
        if (lhs->text != accName) {
            return false;
        }
        // RHS is stored as lhs->right
        AbstractSyntaxTreePtr rhs = lhs->right;
        if (!rhs) {
            return false;
        }
        // Reject complex data shapes / complex stride
        if (stride != 1) {
            return false;
        }
        // Reject if RHS contains any function-call nodes
        std::function<bool(AbstractSyntaxTreePtr)> containsCall
            = [&](AbstractSyntaxTreePtr n) -> bool {
            for (AbstractSyntaxTreePtr c = n; c != nullptr; c = c->right) {
                if (c->opNum == OP_SCALL || c->opNum == OP_MULTICALL) {
                    return true;
                }
                if (c->down && containsCall(c->down)) {
                    return true;
                }
            }
            return false;
        };
        if (containsCall(rhs)) {
            return false;
        }
        // RHS must include both accName and idxName and no other identifier names
        bool accFound = false;
        bool idxFound = false;
        bool otherIdFound = false;
        std::function<void(AbstractSyntaxTreePtr)> scanIds = [&](AbstractSyntaxTreePtr n) {
            for (AbstractSyntaxTreePtr c = n; c != nullptr; c = c->right) {
                if (c->type == id_node) {
                    if (c->text == accName) {
                        accFound = true;
                    } else if (c->text == idxName) {
                        idxFound = true;
                    } else {
                        otherIdFound = true;
                    }
                }
                if (c->down) {
                    scanIds(c->down);
                }
            }
        };
        scanIds(rhs);
        if (!accFound || !idxFound || otherIdFound) {
            return false;
        }
        // Finally, only allow plus/minus operators in the RHS expression tree
        std::function<bool(AbstractSyntaxTreePtr)> onlyPlusMinus
            = [&](AbstractSyntaxTreePtr n) -> bool {
            for (AbstractSyntaxTreePtr c = n; c != nullptr; c = c->right) {
                if (c->type == non_terminal
                    && !(c->opNum == OP_PLUS || c->opNum == OP_SUBTRACT || c->opNum == OP_RHS)) {
                    // allow OP_RHS (identifiers / values) and + / - operators
                    return false;
                }
                if (c->down && !onlyPlusMinus(c->down)) {
                    return false;
                }
            }
            return true;
        };
        if (!onlyPlusMinus(rhs)) {
            return false;
        }
        return true;
    };

    // Attempt reduction fast-path only when body cannot modify index and the
    // codeBlock matches the strict reduction pattern.
    if (!bodyMayModifyIndex
        && detectSimpleReduction(codeBlock, /*accName*/ std::string(), indexName)) {
        // NOTE: detectSimpleReduction requires accName; we need to extract it from
        // the single-statement LHS. Do that here (repeat minimal checks).
        AbstractSyntaxTreePtr stmt = codeBlock->down;
        AbstractSyntaxTreePtr lhs = stmt->down;
        const std::string accName = lhs->text;

        // Lookup accumulator variable; conservative: must exist, be scalar numeric
        ArrayOf* accVar = scope->lookupVariable(accName);
        if (accVar && accVar->isScalar()) {
            const NelsonType accClass = accVar->getDataClass();
            // Only support a subset of numeric accumulator types for now
            switch (accClass) {
            case NLS_DOUBLE: {
                double accLocal = static_cast<const double*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<double>(idxData[m]);
                }
                double* dst = (double*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            case NLS_SINGLE: {
                single accLocal = static_cast<const single*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<single>(idxData[m]);
                }
                single* dst = (single*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            case NLS_INT32: {
                int32 accLocal = static_cast<const int32*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<int32>(idxData[m]);
                }
                int32* dst = (int32*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            case NLS_INT64: {
                int64 accLocal = static_cast<const int64*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<int64>(idxData[m]);
                }
                int64* dst = (int64*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            case NLS_UINT32: {
                uint32 accLocal = static_cast<const uint32*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<uint32>(idxData[m]);
                }
                uint32* dst = (uint32*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            case NLS_UINT64: {
                uint64 accLocal = static_cast<const uint64*>(accVar->getDataPointer())[0];
                const T* idxData = indexSet;
                for (indexType m = 0; m < count; ++m) {
                    accLocal += static_cast<uint64>(idxData[m]);
                }
                uint64* dst = (uint64*)(accVar->getDataPointer());
                dst[0] = accLocal;
                return;
            }
            default:
                break;
            }
        }
        // If any precondition fails, fall through to the normal hot path below.
    }

    if (!bodyMayModifyIndex) {
        // Hot path: safe to cache
        ArrayOf* vp = scope->lookupVariable(indexName);
        if ((!vp) || (vp->getDataClass() != indexClass) || (!vp->isScalar())) {
            scope->insertVariable(indexName,
                ArrayOf(
                    indexClass, Dimensions(1, 1), ArrayOf::allocateArrayOf(indexClass, stride)));
            vp = scope->lookupVariable(indexName);
        }
        // Cache data pointer once (valid because body cannot reassign the var)
        T* dst = static_cast<T*>(vp->getReadWriteDataPointer());
        for (indexType m = 0; m < count; ++m) {
            // copy with stride
            dst[0] = indexSet[m * stride];
            if (stride == 2) {
                dst[1] = indexSet[m * stride + 1];
            }
            eval->block(codeBlock);
            int st = eval->getState();
            if (st == NLS_STATE_BREAK) {
                eval->resetState();
                break;
            }
            if (st == NLS_STATE_RETURN || st == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
                break;
            }
            if (st == NLS_STATE_CONTINUE) {
                eval->resetState();
            }
        }
        return;
    }

    // Fallback: safe, semantics-preserving original behavior when the body
    // might reassign the index variable (we must re-query the scope each
    // iteration because user code can replace the variable).
    for (indexType m = 0; m < count; ++m) {
        ArrayOf* vp = scope->lookupVariable(indexName);
        if ((!vp) || (vp->getDataClass() != indexClass) || (!vp->isScalar())) {
            scope->insertVariable(indexName,
                ArrayOf(
                    indexClass, Dimensions(1, 1), ArrayOf::allocateArrayOf(indexClass, stride)));
            vp = scope->lookupVariable(indexName);
        }
        T* dst = static_cast<T*>(vp->getReadWriteDataPointer());
        // copy with stride
        dst[0] = indexSet[m * stride];
        if (stride == 2) {
            dst[1] = indexSet[m * stride + 1];
        }
        eval->block(codeBlock);
        int st = eval->getState();
        if (st == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
        if (st == NLS_STATE_RETURN || st == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (st == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
    }
}
//=============================================================================
// This function handles the row vector case for complex types.
template <class T>
void
ForStatementRowVectorComplexHelper(AbstractSyntaxTreePtr codeBlock, NelsonType indexClass,
    ArrayOf& indexSet, indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    const T* data = (const T*)indexSet.getDataPointer();
    // stride = 2 for complex (real, imag) pairs
    ForLoopHelper<T>(codeBlock, indexClass, data, elementCount, 2, indexVarName, eval);
}
//=============================================================================
template <class T>
void
ForStatementRowVectorHelper(AbstractSyntaxTreePtr codeBlock, NelsonType indexClass,
    ArrayOf& indexSet, indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    const T* data = static_cast<const T*>(indexSet.getDataPointer());
    // stride = 1 for scalar elements
    ForLoopHelper<T>(codeBlock, indexClass, data, elementCount, 1, indexVarName, eval);
}
//=============================================================================
// This function handles the row vector case for non-complex types.
static void
ForStatemenRowVectorGenericHelper(AbstractSyntaxTreePtr codeBlock, ArrayOf& indexSet,
    indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    Context* context = eval->getContext();
    ArrayOf indexVar;
    for (indexType elementNumber = 0; elementNumber < elementCount; ++elementNumber) {
        indexVar = indexSet.getValueAtIndex(elementNumber);
        if (!context->insertVariable(indexVarName, indexVar)) {
            Error(_W("Valid variable name expected."));
        }
        eval->block(codeBlock);
        int st = eval->getState();
        if (st == NLS_STATE_RETURN || st == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (st == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
        if (st == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
    }
}
//=============================================================================
// This function handles the case for matrix indexing in a generic way.
static void
ForStatemenMatrixGenericHelper(AbstractSyntaxTreePtr codeBlock, ArrayOf& indexSet,
    indexType elementCount, const std::string& indexVarName, Evaluator* eval)
{
    ArrayOf indexVar;
    for (indexType elementNumber = 0; elementNumber < elementCount; elementNumber++) {
        indexType tmp = indexSet.getRows();
        ArrayOfVector m;
        m.reserve(2);
        m.push_back(ArrayOf::integerRangeConstructor(1, 1, tmp, false));
        m.push_back(ArrayOf::doubleConstructor((double)(elementNumber + 1)));
        indexVar = indexSet.getNDimSubset(m);
        if (!eval->getContext()->insertVariable(indexVarName, indexVar)) {
            Error(_W("Valid variable name expected."));
        }
        eval->block(codeBlock);
        if (eval->getState() == NLS_STATE_RETURN || eval->getState() == NLS_STATE_ABORT
            || eval->isQuitOrForceQuitState()) {
            break;
        }
        if (eval->getState() == NLS_STATE_CONTINUE) {
            eval->resetState();
        }
        if (eval->getState() == NLS_STATE_BREAK) {
            eval->resetState();
            break;
        }
    }
}
//=============================================================================
class ContextLoopLocker
{
    Context* m_context;

public:
    ContextLoopLocker(Context* a) : m_context(a) { m_context->enterLoop(); }
    ~ContextLoopLocker() { m_context->exitLoop(); }
};
//=============================================================================
// This function handles the for statement for row vectors.
void
Evaluator::forStatement(AbstractSyntaxTreePtr t)
{
    if (!t) {
        resetState();
        context->exitLoop();
        return;
    }
    callstack.pushID((size_t)t->getContext());

    const std::string& indexVarName = t->text;
    // Support the three forms of for-loop:
    // 1) for (var = expr)
    // 2) for var = expr
    // 3) for var   (pre-initialized variable used as index set)
    ArrayOf indexSet;
    AbstractSyntaxTreePtr codeBlock = t->right;

    if (t->down != nullptr) {
        indexSet = expression(t->down);
    } else {
        // No RHS expression: use the current value of the loop variable
        if (!context->lookupVariable(indexVarName, indexSet)) {
            Error(_W("Index variable used in for statement must be defined."));
        }
    }
    if (indexSet.isEmpty()) {
        callstack.popID();
        return;
    }
    if (!IsValidVariableName(indexVarName, true)) {
        Error(_W("Valid variable name expected."));
    }
    codeBlock = t->right;
    const bool isRowVector = indexSet.isRowVector();
    const indexType elementCount = isRowVector
        ? indexSet.getElementCount()
        : (indexSet.isColumnVector() ? 1 : indexSet.getColumns());

    ContextLoopLocker loopLocker(context);
    if (isRowVector) {
        switch (indexSet.getDataClass()) {
        case NLS_LOGICAL:
            ForStatementRowVectorHelper<logical>(
                codeBlock, NLS_LOGICAL, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_UINT8:
            ForStatementRowVectorHelper<uint8>(
                codeBlock, NLS_UINT8, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_INT8:
            ForStatementRowVectorHelper<int8>(
                codeBlock, NLS_INT8, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_UINT16:
            ForStatementRowVectorHelper<uint16>(
                codeBlock, NLS_UINT16, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_INT16:
            ForStatementRowVectorHelper<int16>(
                codeBlock, NLS_INT16, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_UINT32:
            ForStatementRowVectorHelper<uint32>(
                codeBlock, NLS_UINT32, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_INT32:
            ForStatementRowVectorHelper<int32>(
                codeBlock, NLS_INT32, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_UINT64:
            ForStatementRowVectorHelper<uint64>(
                codeBlock, NLS_UINT64, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_INT64:
            ForStatementRowVectorHelper<int64>(
                codeBlock, NLS_INT64, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_SINGLE:
            ForStatementRowVectorHelper<single>(
                codeBlock, NLS_SINGLE, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_DOUBLE:
            ForStatementRowVectorHelper<double>(
                codeBlock, NLS_DOUBLE, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_SCOMPLEX:
            ForStatementRowVectorComplexHelper<single>(
                codeBlock, NLS_SCOMPLEX, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_DCOMPLEX:
            ForStatementRowVectorComplexHelper<double>(
                codeBlock, NLS_DCOMPLEX, indexSet, elementCount, indexVarName, this);
            break;
        case NLS_CHAR:
            ForStatementRowVectorHelper<charType>(
                codeBlock, NLS_CHAR, indexSet, elementCount, indexVarName, this);
            break;
        default:
            ForStatemenRowVectorGenericHelper(
                codeBlock, indexSet, elementCount, indexVarName, this);
            break;
        }
    } else {
        ForStatemenMatrixGenericHelper(codeBlock, indexSet, elementCount, indexVarName, this);
    }

    callstack.popID();
}
//=============================================================================
bool
Evaluator::conditionedStatement(AbstractSyntaxTreePtr t)
{
    bool conditionState;
    if (t->opNum != OP_CSTAT) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    AbstractSyntaxTreePtr s = t->down;
    callstack.pushID((size_t)s->getContext());
    ArrayOf condVar;
    condVar = expression(s);
    conditionState = checkIfWhileCondition(condVar);
    AbstractSyntaxTreePtr codeBlock = s->right;
    if (conditionState) {
        block(codeBlock);
    }
    callstack.popID();
    return conditionState;
}
//=============================================================================
/**
 * This somewhat strange test is used by the switch statement.
 * If x is a scalar, and we are a scalar, this is an equality
 * test.  If x is a string and we are a string, this is a
 * strcmp test.  If x is a scalar and we are a cell-array, this
 * test is applied on an element-by-element basis, looking for
 * any matches.  If x is a string and we are a cell-array, then
 * this is applied on an element-by-element basis also.
 */
bool
Evaluator::testCaseStatement(AbstractSyntaxTreePtr t, const ArrayOf& s)
{
    bool caseMatched;
    ArrayOf r;
    callstack.pushID((size_t)t->getContext());
    if (t->type != reserved_node || t->tokenNumber != NLS_KEYWORD_CASE) {
        Error(ERROR_AST_SYNTAX_ERROR);
    }
    t = t->down;
    r = expression(t);
    caseMatched = s.testForCaseMatch(r);
    if (caseMatched) {
        block(t->right);
    }
    callstack.popID();
    return caseMatched;
}
//=============================================================================
//!
//@Module TRY-CATCH Try and Catch Statement
//@@Section FLOW
//@@Usage
// The @|try| and @|catch| statements are used for error handling
// and control.  A concept present in @|C++|, the @|try| and @|catch|
// statements are used with two statement blocks as follows
//@[
//   try
//     statements_1
//   catch
//     statements_2
//   end
//@]
// The meaning of this construction is: try to execute @|statements_1|,
// and if any errors occur during the execution, then execute the
// code in @|statements_2|.  An error can either be a Nelson generated
// error (such as a syntax error in the use of a built in function), or
// an error raised with the @|error| command.
//@@Examples
// Here is an example of a function that uses error control via @|try|
// and @|catch| to check for failures in @|fopen|.
//@{ read_file.m
// function c = read_file(filename)
// try
//   fp = fopen(filename,'r');
//   c = fgetline(fp);
//   fclose(fp);
// catch
//   c = ['could not open file because of error :' lasterr]
// end
//@}
// Now we try it on an example file - first one that does not exist,
// and then on one that we create (so that we know it exists).
//@<
// read_file('this_filename_is_invalid')
// fp = fopen('test_text.txt','w');
// fprintf(fp,'a line of text\n');
// fclose(fp);
// read_file('test_text.txt')
//@>
//!
void
Evaluator::tryStatement(AbstractSyntaxTreePtr t)
{
    size_t stackdepth = callstack.size();

    try {
        block(t);
    } catch (const Exception& e) {
        // Optimize stack unwinding by using a single call to `resize` instead of a loop
        if (callstack.size() > stackdepth) {
            callstack.setSize(stackdepth);
        }

        t = t->right;
        if (t != nullptr) {
            if (t->type == id_node) {
                this->context->insertVariable(t->text, ExceptionToArrayOf(e));
                t = t->down;
            }
            if (t != nullptr) {
                block(t);
            }
        }
    }
}
//=============================================================================
//!
//@Module SWITCH Switch statement
//@@Section FLOW
//@@Usage
// The @|switch| statement is used to selective execute code
// based on the value of either scalar value or a string.
// The general syntax for a @|switch| statement is
//@[
//  switch(expression)
//    case test_expression_1
//      statements
//    case test_expression_2
//      statements
//    otherwise:
//      statements
//  end
//@]
// The @|otherwise| clause is optional.  Note that each test
// expression can either be a scalar value, a string to test
// against (if the switch expression is a string), or a
//@|cell-array| of expressions to test against.  Note that
// unlike @|C| @|switch| statements, the Nelson @|switch|
// does not have fall-through, meaning that the statements
// associated with the first matching case are executed, and
// then the @|switch| ends.  Also, if the @|switch| expression
// matches multiple @|case| expressions, only the first one
// is executed.
//@@Examples
// Here is an example of a @|switch| expression that tests
// against a string input:
//@{ switch_test.m
// function c = switch_test(a)
//  switch(a)
//    case {'lima beans','root beer'}
//      c = 'food';
//    case {'red','green','blue'}
//      c = 'color';
//    otherwise
//      c = 'not sure';
//  end
//@}
// Now we exercise the switch statements
//@<
// switch_test('root beer')
// switch_test('red')
// switch_test('carpet')
//@>
//!
void
Evaluator::switchStatement(AbstractSyntaxTreePtr t)
{
    ArrayOf switchVal;
    callstack.pushID(t->getContext());
    // First, extract the value to perform the switch on.
    switchVal = expression(t);
    // Assess its type to determine if this is a scalar switch
    // or a string switch.
    if (!switchVal.isScalar() && !switchVal.isRowVectorCharacterArray()) {
        Error(ERROR_SWITCH_STATEMENTS);
    }
    // Move to the next node in the AST
    t = t->right;
    // Check for additional conditions
    if (t != nullptr) {
        bool caseMatched = false;
        if (t->opNum == (OP_CASEBLOCK)) {
            AbstractSyntaxTreePtr s = t->down;
            while (!caseMatched && s != nullptr) {
                caseMatched = testCaseStatement(s, switchVal);
                s = s->right;
            }
        }
        t = t->right;
        if (!(caseMatched || (t == nullptr)))
        // Do the "otherwise" code
        {
            block(t);
        }
    }
    callstack.popID();
}
//=============================================================================
void
Evaluator::statementType(AbstractSyntaxTreePtr t, bool printIt)
{
    if (t == nullptr) {
        return;
    }

    if (haveEventsLoop()) {
        ProcessEventsDynamicFunctionWithoutWait();
    }
    if (!commandQueue.isEmpty()) {
        std::wstring cmd;
        commandQueue.get(cmd);
        evaluateString(cmd);
    }

    callstack.pushID((size_t)t->getContext());

    if (t->isEmpty()) {
        callstack.popID();
        return;
    }

    switch (t->opNum) {
    case OP_ASSIGN:
        assignStatement(t->down, printIt);
        callstack.popID();
        return;
    case OP_MULTICALL:
        multiFunctionCall(t->down, printIt);
        callstack.popID();
        return;
    case OP_SCALL: {
        ArrayOfVector m = specialFunctionCall(t->down, printIt);
        if (!m.empty()) {
            context->insertVariable("ans", m[0]);
            display(m[0], "ans", false, true);
        }
        callstack.popID();
        return;
    }
    case OP_RHS: {
        ArrayOf b;
        ArrayOfVector m;
        bool bUpdateAns = false;
        FunctionDef* fdef = nullptr;
        if (!context->lookupVariable(t->down->text, b) && lookupFunction(t->down->text, fdef)) {
            m = functionExpression(fdef, t->down, 0, true);
            if (!m.empty()) {
                b = m[0];
                bUpdateAns = true;
            }
            if (printIt && !m.empty() && state < NLS_STATE_QUIT) {
                display(b, "ans", false, true);
            }
        } else {
            if (context->lookupVariable(t->down->text, b) && b.isFunctionHandle()) {
                m = rhsExpression(t->down, 0);
            } else {
                if (b.isCell()) {
                    try {
                        m = rhsExpression(t->down);
                    } catch (Exception& e) {
                        if (!e.matches(ERROR_EMPTY_EXPRESSION)) {
                            throw;
                        }
                    }
                } else {
                    m = rhsExpression(t->down);
                }
            }
            if (m.empty()) {
                b = ArrayOf::emptyConstructor();
            } else {
                b = m[0];
                if (printIt && state < NLS_STATE_QUIT) {
                    if (b.name().empty()) {
                        bUpdateAns = true;
                    }
                    for (size_t j = 0; j < m.size(); j++) {
                        if (m.size() > 1) {
                            std::string message = fmt::format(_("\n{} of {}:\n"),
                                static_cast<int>(j) + 1, static_cast<int>(m.size()));
                            io->outputMessage(message);
                        }
                        display(m[j], m[j].name().empty() ? "ans" : m[j].name(), false, true);
                    }
                }
            }
        }
        if (isQuitOrForceQuitState() || state == NLS_STATE_ABORT) {
            callstack.popID();
            return;
        }
        if (bUpdateAns) {
            context->insertVariable("ans", b);
        }
        callstack.popID();
        return;
    }
    default:
        break;
    }

    if (t->type == reserved_node) {
        switch (t->tokenNumber) {
        case NLS_KEYWORD_FOR:
            forStatement(t->down);
            break;
        case NLS_KEYWORD_WHILE:
            whileStatement(t->down);
            break;
        case NLS_KEYWORD_IF:
            ifStatement(t->down);
            break;
        case NLS_KEYWORD_BREAK:
            if (context->inLoop()) {
                state = NLS_STATE_BREAK;
            }
            break;
        case NLS_KEYWORD_CONTINUE:
            if (context->inLoop()) {
                state = NLS_STATE_CONTINUE;
            }
            break;
        case NLS_KEYWORD_RETURN:
            state = NLS_STATE_RETURN;
            break;
        case NLS_KEYWORD_SWITCH:
            switchStatement(t->down);
            break;
        case NLS_KEYWORD_TRY:
            tryStatement(t->down);
            break;
        case NLS_KEYWORD_ABORT:
            state = NLS_STATE_ABORT;
            depth = 0;
            break;
        case NLS_KEYWORD_KEYBOARD:
            depth++;
            evalCLI();
            if (state < NLS_STATE_QUIT) {
                resetState();
            }
            depth--;
            break;
        case NLS_KEYWORD_ENDFUNCTION:
            if (context->getCurrentScope()->getName() == "base") {
                Error(ERROR_ENDFUNCTION_WRONG_USE);
            }
            state = NLS_STATE_RETURN;
            break;
        default:
            Error(ERROR_UNRECOGNIZED_STATEMENT);
        }
        callstack.popID();
        return;
    }

    ArrayOf b = expression(t);
    if (printIt && state < NLS_STATE_QUIT) {
        display(b, "ans", false, true);
    }
    if (isQuitOrForceQuitState() || state == NLS_STATE_ABORT) {
        callstack.popID();
        return;
    }
    context->insertVariable("ans", b);
    callstack.popID();
}
//=============================================================================
// Trapping at the statement level is much better! - two
// problems... try/catch and multiline statements (i.e.,atell.m)
// The try-catch one is easy, I think...  When a try occurs,
// we capture the stack depth... if an exception occurs, we
// unwind the stack to this depth..
// The second one is trickier - suppose we have a conditional
// statement
// if (a == 3)
//    bfunc
// else
//    cfunc
// end
// this is represented in the parse tree as a single construct...
//
void
Evaluator::statement(AbstractSyntaxTreePtr t)
{
    bool popNeeded = false;
    try {
        // For control flow statements (for, while, if, switch), use the corrected line number
        // instead of the node's context which points to the end keyword
        size_t contextToPush = (size_t)t->getContext();
        if (t->opNum == OP_RSTATEMENT || t->opNum == OP_QSTATEMENT) {
            if (t->down != nullptr && t->down->type == reserved_node) {
                if (t->down->tokenNumber == NLS_KEYWORD_FOR
                    || t->down->tokenNumber == NLS_KEYWORD_WHILE
                    || t->down->tokenNumber == NLS_KEYWORD_IF
                    || t->down->tokenNumber == NLS_KEYWORD_SWITCH) {
                    // Get the corrected line position using getLinePosition
                    contextToPush = getLinePosition(t);
                }
            }
        }

        callstack.pushID(contextToPush);
        popNeeded = true;

        if (onBreakpoint(t)) {

            // Ensure debug prompt shows breakpoint mode on first entry
            bpActive = true;

            while (true) {
                debugCLI();

                if (state == NLS_STATE_ABORT) {
                    bpActive = false;
                    return;
                }

                // Handle quit/exit command during debug session
                if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
                    bpActive = false;
                    resetDebugDepth();
                    return;
                }

                if (state == NLS_STATE_DEBUG_QUIT_ALL) {
                    bpActive = false;
                    state = NLS_STATE_ABORT;
                    return;
                }

                if (state == NLS_STATE_DEBUG_QUIT) {
                    callstack.popID();
                    state = NLS_STATE_ABORT;
                    return;
                }
                if (state == NLS_STATE_DEBUG_CONTINUE || state == NLS_STATE_DEBUG_STEP) {
                    if (state == NLS_STATE_DEBUG_STEP) {
                        bpActive = true;
                    } else {
                        bpActive = false;
                    }
                    state = NLS_STATE_OK;
                    break;
                }
            }
        }

        switch (t->opNum) {
        case OP_QSTATEMENT:
            statementType(t->down, false);
            break;
        case OP_RSTATEMENT:
            statementType(t->down, bEchoMode);
            break;
        default:
            break;
        }
    } catch (const Exception&) {
        if (popNeeded) {
            callstack.popID();
        }
        throw;
    }
    if (popNeeded) {
        callstack.popID();
    }
}
//=============================================================================
// This function executes a block of code represented by an AbstractSyntaxTree.
void
Evaluator::block(AbstractSyntaxTreePtr t)
{
    if (!t) {
        return;
    }

    try {
        // Collect children once to avoid walking linked AST pointers repeatedly.
        std::vector<AbstractSyntaxTreePtr> children;
        for (AbstractSyntaxTreePtr cur = t->down; cur != nullptr; cur = cur->right) {
            children.push_back(cur);
        }

        if (state < NLS_STATE_QUIT) {
            resetState();
        }

        NelsonConfiguration* cfg = NelsonConfiguration::getInstance();

        for (size_t i = 0; i < children.size(); ++i) {
            AbstractSyntaxTreePtr s = children[i];

            if (state >= NLS_STATE_QUIT && state != NLS_STATE_CANCEL_QUIT) {
                break;
            }

            // Check interrupt pending via cached pointer
            if (cfg->getInterruptPending(ID)) {
                if (ID == 0) {
                    cfg->setInterruptPending(false, ID);
                    CallbackQueue::getInstance()->clear();
                    EventQueue::getInstance()->clear();
                    setState(NLS_STATE_ABORT);
                    Error(MSG_CTRL_C_DETECTED);
                } else {
                    Error(_W("Execution of the future was cancelled."),
                        L"parallel:fevalqueue:ExecutionCancelled");
                }
                break;
            }

            statement(s);

            if (state == NLS_STATE_BREAK || state == NLS_STATE_CONTINUE || state == NLS_STATE_RETURN
                || state == NLS_STATE_ABORT || isQuitOrForceQuitState()) {
                break;
            }
        }
    } catch (Exception& e) {
        if (!e.isEmpty()) {
            setLastErrorException(e);
            throw;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
