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
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Operators.hpp"
#include "OverloadName.hpp"
#include "IsValidVariableName.hpp"
#include "Warning.hpp"
#include "HandleGenericObject.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
getArgumentIndex(const stringVector& list, const std::string& t)
{
    bool foundArg = false;
    std::string q;
    size_t i = 0;
    while (i < list.size() && !foundArg) {
        q = list[i];
        if (q[0] == '&') {
            q.erase(0, 1);
        }
        foundArg = (q == t);
        if (!foundArg) {
            i++;
        }
    }
    if (foundArg) {
        return static_cast<int>(i);
    }
    return -1;
}
//=============================================================================
//!
//@Module SPECIAL Special Calling Syntax
//@@Section FUNCTIONS
//@@Usage
// To reduce the effort to call certain functions, Nelson supports
// a special calling syntax for functions that take string arguments.
// In particular, the three following syntaxes are equivalent, with
// one caveat:
//@[
//   functionname('arg1','arg2',...,'argn')
//@]
// or the parenthesis and commas can be removed
//@[
//   functionname 'arg1' 'arg2' ... 'argn'
//@]
// The quotes are also optional (providing, of course, that the
// argument strings have no spaces in them)
//@[
//   functionname arg1 arg2 ... argn
//@]
// This special syntax enables you to type @|hold on| instead of
// the more cumbersome @|hold('on')|.  The caveat is that Nelson
// currently only recognizes the special calling syntax as the
// first statement on a line of input.  Thus, the following construction
//@[
//  for i=1:10; plot(vec(i)); hold on; end
//@]
// would not work.  This limitation may be removed in a future
// version.
//@@Example
// Here is a function that takes two string arguments and
// returns the concatenation of them.
//@{ strcattest.m
// function strcattest(str1,str2)
//  str3 = [str1,str2];
//  printf('str1 = %s, str2 = %s, str3 = %s\n',str1,str2,str3);
//@}
// We call @|strcattest| using all three syntaxes.
//@<
// strcattest('hi','ho')
// strcattest 'hi' 'ho'
// strcattest hi ho
//@>
//!
ArrayOfVector
Evaluator::functionExpression(
    FunctionDef* funcDef, AbstractSyntaxTreePtr t, int narg_out, bool outputOptional)
{
    ArrayOfVector m;
    ArrayOfVector n;
    AbstractSyntaxTreePtr s = nullptr;
    AbstractSyntaxTreePtr q = nullptr;
    AbstractSyntaxTreePtr p = nullptr;
    stringVector keywords;
    ArrayOfVector keyvals;
    AbstractSyntaxTreePtrVector keyexpr;
    int* keywordNdx = nullptr;
    int* argTypeMap = nullptr;
    callstack.pushID(t->getContext());
    bool CLIFlagsave = InCLI;
    try {
        {
            // Look for arguments
            if (t->down != nullptr) {
                s = t->down;
                if (s->opNum == (OP_PARENS)) {
                    s = s->down;
                    // Search for the keyword uses -
                    // To handle keywords, we make one pass through the arguments,
                    // recording a list of keywords used and using ::expression to
                    // evaluate their values.
                    q = s;
                    while (q != nullptr) {
                        if (q->opNum == OP_KEYWORD) {
                            keywords.push_back(q->down->text);
                            if (q->down->right != nullptr) {
                                keyvals.push_back(expression(q->down->right));
                            } else {
                                keyvals.push_back(ArrayOf::logicalConstructor(true));
                            }
                            keyexpr.push_back(q->down->right);
                        }
                        q = q->right;
                    }
                    // If any keywords were found, make another pass through the
                    // arguments and remove them.
                    m = expressionList(s);
                    // Check for keywords
                    if (keywords.size() > 0) {
                        // If keywords were used, we have to permute the
                        // entries of the arrayvector to the correct order.
                        stringVector arguments;
                        // Get the arguments from the MacroFunction pointer.
                        arguments = funcDef->arguments;
                        try {
                            keywordNdx = new int[keywords.size()];
                        } catch (std::bad_alloc&) {
                            raiseError(L"Nelson:error_manager:no_mem", ERROR_MEMORY_ALLOCATION);
                        }
                        int maxndx;
                        maxndx = 0;
                        // Map each keyword to an argument number
                        for (size_t i = 0; i < keywords.size(); ++i) {
                            int ndx;
                            ndx = getArgumentIndex(arguments, keywords[i]);
                            if (ndx == -1) {
                                if (keywordNdx) {
                                    delete[] keywordNdx;
                                    keywordNdx = nullptr;
                                }
                                raiseError(L"Nelson:interpreter:ERROR_OUT_OF_ORDER_ARGUMENT",
                                    ERROR_OUT_OF_ORDER_ARGUMENT, utf8_to_wstring(keywords[i]));
                            }
                            keywordNdx[i] = ndx;
                            if (ndx > maxndx) {
                                maxndx = ndx;
                            }
                        }
                        // Next, we have to determine how many "holes" there are
                        // in the argument list - we get the maximum list
                        size_t holes = maxndx + 1 - keywords.size();
                        // At this point, holes is the number of missing arguments
                        // If holes > m.size(), then the total number of arguments
                        // is just maxndx+1.  Otherwise, its
                        // maxndx+1+(m.size() - holes)
                        size_t totalCount;
                        if (holes > m.size()) {
                            totalCount = ((size_t)maxndx + (size_t)1);
                        } else {
                            totalCount = (size_t)(maxndx + 1 + (m.size() - holes));
                        }
                        // Next, we allocate a vector to hold the values
                        ArrayOfVector toFill(totalCount);
                        bool* filled = new bool[totalCount];
                        argTypeMap = new int[totalCount];
                        for (size_t i = 0; i < totalCount; i++) {
                            filled[i] = false;
                            argTypeMap[i] = -1;
                        }
                        // Finally...
                        // Copy the keyword values in
                        for (size_t i = 0; i < keywords.size(); i++) {
                            toFill[keywordNdx[i]] = keyvals[i];
                            filled[keywordNdx[i]] = true;
                            argTypeMap[keywordNdx[i]] = (int)i;
                        }
                        // Fill out the rest of the values from m
                        size_t nidx = 0;
                        int pidx = 0;
                        while (nidx < m.size()) {
                            if (!filled[pidx]) {
                                toFill[pidx] = m[nidx];
                                filled[pidx] = true;
                                argTypeMap[pidx] = -2;
                                nidx++;
                            }
                            pidx++;
                        }
                        // Finally, fill in empty matrices for the
                        // remaining arguments
                        for (size_t i = 0; i < totalCount; i++) {
                            if (!filled[i]) {
                                toFill[i] = ArrayOf::emptyConstructor();
                            }
                        }
                        // Clean up
                        delete[] filled;
                        // delete[] keywordNdx;
                        // Reassign
                        m = toFill;
                    }
                } else {
                    ArrayOf r;
                    bool isVar = context->lookupVariable(t->text, r);
                    if (isVar) {
                        if (r.isClassType()) {
                            s = t->down;
                            if (s->opNum == (OP_DOT) || s->opNum == (OP_DOTDYN)) {
                                s = s->down;
                                return scalarArrayOfToArrayOfVector(r.getField(s->text));
                            }
                        }
                    }
                    // exception for "py" name, need to be more generic
                    if (t->text == "py") {
                        FunctionDefPtr fdef;
                        if (BuiltInFunctionDefManager::getInstance()->find(t->text, fdef)) {
                            ArrayOfVector inputs;
                            return fdef->evaluateFunction(this, inputs, 1);
                        }
                    }
                    raiseError(L"Nelson:interpreter:ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION",
                        ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION);
                }
            } else {
                m = ArrayOfVector();
            }
            CLIFlagsave = InCLI;
            InCLI = false;
            ArrayOf r;
            bool isVar = context->lookupVariable(t->text, r);
            bool haveDown = (t->down != nullptr);
            if (isVar) {
                // if it is a class C
                if (r.isClassType() || r.isFunctionHandle()) {
                    // C(X1, ..., XN)
                    // we call : C_extract(C, X1, ..., XN)
                    if (m.size() > 0) {
                        m.push_front(r);
                    } else {
                        // C() or C
                        if (t->down != nullptr) {
                            s = t->down;
                            // C()
                            if (s->opNum == (OP_PARENS)) {
                                m.push_front(r);
                            }
                            // C others ? C{}, C. ?
                            // we do currently nothing
                        } else {
                            if (keywordNdx != nullptr) {
                                delete[] keywordNdx;
                                keywordNdx = nullptr;
                            }
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            return scalarArrayOfToArrayOfVector(r);
                        }
                    }
                }
                n = funcDef->evaluateFunction(this, m, narg_out);
                // Check for step-out breakpoint after function returns
                if (checkStepOutAfterFunctionReturn(t)) {
                    // Enter debug CLI at the call site
                    while (true) {
                        debugCLI();
                        if (state == NLS_STATE_ABORT || state == NLS_STATE_DEBUG_QUIT_ALL
                            || state == NLS_STATE_DEBUG_QUIT) {
                            bpActive = false;
                            break;
                        }
                        // Handle quit/exit command during debug session
                        if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
                            bpActive = false;
                            resetDebugDepth();
                            break;
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
            } else {
                if (funcDef == nullptr) {
                    if (m.size() > 0 && m[0].isScalar()
                        && (m[0].isClassType() || m[0].isHandle())) {

                        if (m[0].isClassType()) {
                            FunctionDefPtr fMethodptr = nullptr;
                            if (!context->lookupFunction("ismethod", fMethodptr)) {
                                std::string msg = t->text;
                                if (msg.empty()) {
                                    raiseError(L"Nelson:interpreter:UNDEFINED_METHOD_NO_NAME",
                                        ERROR_UNDEFINED_METHOD_NO_NAME);
                                } else {
                                    raiseError(L"Nelson:interpreter:UNDEFINED_METHOD",
                                        ERROR_UNDEFINED_METHOD, utf8_to_wstring(msg));
                                }
                            }
                            ArrayOfVector OutputResult;
                            ArrayOf methodName = ArrayOf::characterArrayConstructor(t->text);
                            ArrayOfVector inputArgs;
                            inputArgs << m[0];
                            inputArgs << methodName;

                            OutputResult = fMethodptr->evaluateFunction(this, inputArgs, 1);
                            if (OutputResult.size() == 1 && OutputResult[0].isLogical()
                                && OutputResult[0].getContentAsLogicalScalar()) {
                                std::string overloadFuncName = OVERLOAD_SYMBOL_STR
                                    + m[0].getClassType() + OVERLOAD_SEPARATOR_CHAR + t->text;
                                if (!context->lookupFunction(overloadFuncName, fMethodptr)) {
                                    std::string msg = t->text;
                                    if (msg.empty()) {
                                        raiseError(L"Nelson:interpreter:UNDEFINED_METHOD_NO_NAME",
                                            ERROR_UNDEFINED_METHOD_NO_NAME);
                                    } else {
                                        raiseError(L"Nelson:interpreter:UNDEFINED_METHOD",
                                            ERROR_UNDEFINED_METHOD, utf8_to_wstring(msg));
                                    }
                                }
                                n = fMethodptr->evaluateFunction(this, m, narg_out);
                            }

                        } else { // HANDLE
                            HandleGenericObject* obj = m[0].getContentAsHandleScalar();
                            if (obj && !obj->invokeMethod(this->io, m, narg_out, t->text, n)) {
                                std::string msg = t->text;
                                if (msg.empty()) {
                                    raiseError(L"Nelson:interpreter:UNDEFINED_METHOD_NO_NAME",
                                        ERROR_UNDEFINED_METHOD_NO_NAME);
                                } else {
                                    raiseError(L"Nelson:interpreter:UNDEFINED_METHOD",
                                        ERROR_UNDEFINED_METHOD, utf8_to_wstring(msg));
                                }
                            }
                        }
                    } else {
                        std::string msg = t->text;
                        if (msg.empty()) {
                            raiseError(L"Nelson:interpreter:UndefinedFunctionOrVariable",
                                ERROR_UNDEFINED_FUNCTION_OR_VARIABLE_NO_NAME);
                        } else {
                            raiseError(L"Nelson:interpreter:UndefinedFunctionOrVariable",
                                ERROR_UNDEFINED_FUNCTION_OR_VARIABLE, utf8_to_wstring(msg));
                        }
                    }
                } else {
                    n = funcDef->evaluateFunction(this, m, narg_out);
                    // Check for step-out breakpoint after function returns
                    if (checkStepOutAfterFunctionReturn(t)) {
                        // Enter debug CLI at the call site
                        while (true) {
                            debugCLI();
                            if (state == NLS_STATE_ABORT || state == NLS_STATE_DEBUG_QUIT_ALL
                                || state == NLS_STATE_DEBUG_QUIT) {
                                bpActive = false;
                                break;
                            }
                            // Handle quit/exit command during debug session
                            if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
                                bpActive = false;
                                resetDebugDepth();
                                break;
                            }
                            if (state == NLS_STATE_DEBUG_CONTINUE
                                || state == NLS_STATE_DEBUG_STEP) {
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
                }
            }
            InCLI = CLIFlagsave;
            if (state == NLS_STATE_ABORT) {
                if (keywordNdx != nullptr) {
                    delete[] keywordNdx;
                    keywordNdx = nullptr;
                }
                if (argTypeMap != nullptr) {
                    delete[] argTypeMap;
                    argTypeMap = nullptr;
                }
                return n;
            }
            // Check for any pass by reference
            if (!haveDown && (funcDef->arguments.size() > 0)) {
                // Get the argument list
                stringVector arguments;
                arguments = funcDef->arguments;
                // M functions can modify their arguments
                size_t maxsearch = m.size();
                if (maxsearch > arguments.size()) {
                    maxsearch = arguments.size();
                }
                if (maxsearch != 0) {
                    q = s;
                }
                for (size_t i = 0; i < maxsearch; i++) {
                    // Was this argument passed out of order?
                    if (argTypeMap != nullptr) {
                        if ((keywords.size() > 0) && (argTypeMap[i] == -1)) {
                            continue;
                        }
                        if ((keywords.size() > 0) && (argTypeMap[i] >= 0)) {
                            p = keyexpr[argTypeMap[i]];
                        } else {
                            p = q;
                            if (q != nullptr) {
                                q = q->right;
                            }
                        }
                    }
                    std::string args(arguments[i]);
                    if (args[0] == '&') {
                        args.erase(0, 1);
                        // This argument was passed by reference
                        if (p == nullptr) {
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            raiseError(L"Nelson:interpreter:ERROR_MUST_HAVE_LVALUE",
                                ERROR_MUST_HAVE_LVALUE);
                            return {};
                        }
                        if (!(p->type == non_terminal && p->opNum == OP_RHS)) {
                            if (argTypeMap != nullptr) {
                                delete[] argTypeMap;
                                argTypeMap = nullptr;
                            }
                            raiseError(L"Nelson:interpreter:ERROR_MUST_HAVE_LVALUE",
                                ERROR_MUST_HAVE_LVALUE);
                            return {};
                        }
                        std::string variableName = p->down->text;
                        ArrayOf c;
                        if (p->down->down == nullptr && p->down->type == id_node) {
                            c = m[i];
                        } else {
                            c = assignExpression(p->down, m[i]);
                        }
                        ArrayOf* ptrVar = context->lookupVariable(variableName);
                        if (ptrVar != nullptr) {
                            ptrVar->setValue(c);
                        } else {
                            c.name(variableName);
                            if (!context->insertVariable(variableName, c)) {
                                if (argTypeMap != nullptr) {
                                    delete[] argTypeMap;
                                    argTypeMap = nullptr;
                                }
                                if (IsValidVariableName(variableName, true)) {
                                    raiseError(
                                        L"Nelson:interpreter:ERROR_REDEFINING_PERMANENT_VARIABLE",
                                        ERROR_REDEFINING_PERMANENT_VARIABLE);
                                }
                                raiseError(L"Nelson:interpreter:ERROR_VALID_VARIABLE_NAME_EXPECTED",
                                    ERROR_VALID_VARIABLE_NAME_EXPECTED);
                                return {};
                            }
                        }
                    }
                }
            }
        }
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        throw;
    }
    callstack.popID();
    if (keywordNdx != nullptr) {
        delete[] keywordNdx;
        keywordNdx = nullptr;
    }
    if (argTypeMap != nullptr) {
        delete[] argTypeMap;
        argTypeMap = nullptr;
    }
    return n;
}
//=============================================================================
void
Evaluator::multiFunctionCall(AbstractSyntaxTreePtr t, bool printIt)
{
    ArrayOfVector m;
    AbstractSyntaxTreePtr s;
    AbstractSyntaxTreePtr fAST;
    AbstractSyntaxTreePtr saveLHS;
    AbstractSyntaxTreePtr cAST;
    ArrayOf c;
    FunctionDef* fptr;
    cAST = t;
    fAST = t->right;
    bool bDeal = false;
    callstack.pushID(fAST->getContext());
    ArrayOf r;
    if (!lookupFunction(fAST->text, fptr)) {
        bool isVar = context->lookupVariable(fAST->text, r);
        if (isVar) {
            if (r.isFunctionHandle()) {
                std::string extractionFunctionName
                    = getOverloadFunctionName(NLS_FUNCTION_HANDLE_STR, SUBSREF_OPERATOR_STR);
                bool isFun = lookupFunction(extractionFunctionName, fptr);
                if (!isFun) {
                    raiseError(L"Nelson:interpreter:ERROR_UNDEFINED_FUNCTION",
                        ERROR_UNDEFINED_FUNCTION, utf8_to_wstring(extractionFunctionName));
                }
            } else if (r.isClassType()) {
                std::string className = r.getClassType();
                std::string extractionFunctionName
                    = getOverloadFunctionName(className, SUBSREF_OPERATOR_STR);
                bool isFun = lookupFunction(extractionFunctionName, fptr);
                if (!isFun) {
                    raiseError(L"Nelson:interpreter:ERROR_UNDEFINED_FUNCTION",
                        ERROR_UNDEFINED_FUNCTION, utf8_to_wstring(extractionFunctionName));
                }
            } else if (r.isCell()) {
                // C = {rand(3),nan(3),zeros(3),inf(3)}
                // [a, b, c, d] = C{ : }
                if (t->opNum != OP_BRACKETS) {
                    raiseError(L"Nelson:interpreter:ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION",
                        ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
                }
                bDeal = true;
            } else {
                raiseError(L"Nelson:interpreter:ERROR_UNDEFINED_FUNCTION", ERROR_UNDEFINED_FUNCTION,
                    utf8_to_wstring(fAST->text));
            }
        } else {
            raiseError(L"Nelson:interpreter:ERROR_UNDEFINED_FUNCTION", ERROR_UNDEFINED_FUNCTION,
                utf8_to_wstring(fAST->text));
        }
    }
    if (t->opNum != OP_BRACKETS) {
        raiseError(L"Nelson:interpreter:ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION",
            ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
    }
    s = t->down;
    if (s->opNum != OP_SEMICOLON) {
        raiseError(L"Nelson:interpreter:ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION",
            ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION);
    }
    if (s->right != nullptr) {
        raiseError(
            L"Nelson:interpreter:ERROR_MULTIPLE_ROWS_NOT_ALLOWED", ERROR_MULTIPLE_ROWS_NOT_ALLOWED);
    }
    // We have to make multiple passes through the LHS part of the AST.
    // The first pass is to count how many function outputs are actually
    // being requested.
    // Calculate how many lhs objects there are
    // lhsSize = s->peerCount();
    s = s->down;
    saveLHS = s;
    // Get the lhs objects into rset
    indexType lhsCount = 0;
    AbstractSyntaxTreePtr mptr = s;
    while (mptr != nullptr) {
        indexType dmp = countLeftHandSides(mptr->down);
        lhsCount += dmp;
        mptr = mptr->right;
    }
    if (bDeal) {
        // C = {rand(3),nan(3),zeros(3),inf(3)}
        // [a, b, c, d] = C{ : }
        Dimensions rhsDimensions;
        rhsDimensions = r.getDimensions();
        m = expressionList(fAST->down->down, r);
        if (m.size() == 0) {
            raiseError(L"Nelson:interpreter:ERROR_INDEX_EXPRESSION_EXPECTED",
                ERROR_INDEX_EXPRESSION_EXPECTED);
        } else if (m.size() == 1) {
            ArrayOfVector m2 = r.getVectorContentsAsList(m[0]);
            if ((indexType)m2.size() < lhsCount) {
                raiseError(L"Nelson:interpreter:ERROR_INSUFFICIENT_NUMBER_OF_OUTPUTS",
                    ERROR_INSUFFICIENT_NUMBER_OF_OUTPUTS);
            } else {
                m = m2;
            }
        } else {
            raiseError(L"Nelson:interpreter:ERROR_CASE_NOT_MANAGED", ERROR_CASE_NOT_MANAGED);
        }
    } else {
        CallStack backupCallStack = callstack;
        m = functionExpression(fptr, fAST, (int)lhsCount, false);
        callstack = backupCallStack;
    }
    s = saveLHS;
    while ((s != nullptr) && (m.size() > 0)) {
        ArrayOf cLocal(assignExpression(s->down, m));
        const bool dropResult = isPlaceholderIdentifier(s->down->text);
        if (!dropResult) {
            if (!context->insertVariable(s->down->text, cLocal)) {
                if (IsValidVariableName(s->down->text, true)) {
                    raiseError(L"Nelson:interpreter:ERROR_REDEFINING_PERMANENT_VARIABLE",
                        ERROR_REDEFINING_PERMANENT_VARIABLE);
                }
                raiseError(L"Nelson:interpreter:ERROR_VALID_VARIABLE_NAME_EXPECTED",
                    ERROR_VALID_VARIABLE_NAME_EXPECTED);
            }
            if (printIt) {
                display(cLocal, s->down->text, false, true);
            }
        }
        s = s->right;
    }
    if (s != nullptr) {
        std::wstring message = _W("Function") + L" : " + utf8_to_wstring(fAST->text) + L"\n"
            + WARNING_OUTPUTS_NOT_ASSIGNED;
        Warning(message);
    }
    callstack.popID();
}
//=============================================================================
ArrayOfVector
Evaluator::specialFunctionCall(AbstractSyntaxTreePtr t, bool printIt)
{
    ArrayOfVector m;
    stringVector args;
    args.push_back(t->text);
    AbstractSyntaxTreePtr s = t->right;
    while (s != nullptr) {
        args.push_back(s->text);
        s = s->right;
    }
    if (args.empty()) {
        return m;
    }
    ArrayOfVector n;
    n.reserve(args.size());
    for (size_t i = 1; i < args.size(); i++) {
        n.push_back(ArrayOf::characterArrayConstructor(args[i]));
    }
    FunctionDefPtr val;
    callstack.pushID((size_t)t->getContext());
    if (!lookupFunction(args[0], val)) {
        raiseError(L"Nelson:interpreter:ERROR_UNABLE_TO_RESOLVE", ERROR_UNABLE_TO_RESOLVE,
            utf8_to_wstring(args[0]));
    }
    bool CLIFlagsave = InCLI;
    InCLI = false;
    try {
        m = val->evaluateFunction(this, n, 0);
    } catch (const Exception&) {
        InCLI = CLIFlagsave;
        callstack.popID();
        throw;
    }
    InCLI = CLIFlagsave;
    callstack.popID();
    return m;
}
//=============================================================================
bool
Evaluator::lookupFunction(const std::string& funcName, FunctionDefPtr& val)
{
    return context->lookupFunction(funcName, val);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
