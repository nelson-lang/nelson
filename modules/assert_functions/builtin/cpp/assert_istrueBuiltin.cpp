//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_istrueBuiltin.hpp"
#include <cmath>
#include <string>
#include "Assert_IsTrue.hpp"
#include "StringPrintf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isMessageArgument(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static bool
isIdentifier(const std::wstring& text)
{
    size_t colon = text.find(L':');
    if (colon == std::wstring::npos || colon == 0 || colon + 1 >= text.size()) {
        return false;
    }
    bool needLetter = true;
    for (wchar_t ch : text) {
        if (ch == L':') {
            needLetter = true;
            continue;
        }
        if (needLetter) {
            if (!((ch >= L'A' && ch <= L'Z') || (ch >= L'a' && ch <= L'z'))) {
                return false;
            }
            needLetter = false;
        } else if (!((ch >= L'A' && ch <= L'Z') || (ch >= L'a' && ch <= L'z')
                       || (ch >= L'0' && ch <= L'9') || ch == L'_')) {
            return false;
        }
    }
    return !needLetter;
}
//=============================================================================
static bool
assertConditionPasses(ArrayOf condition)
{
    if ((!condition.isLogical() && !condition.isNumeric()) || condition.isComplex()) {
        Error(L"Nelson:asserts:invalidArgument", L"Wrong type for argument #%d: %s expected.", (1),
            L"logical");
    }
    if (condition.isSparse()) {
        condition.makeDense();
    }
    indexType count = condition.elementCount();

#define ASSERT_ALL_NON_ZERO(TYPE, NELSON_TYPE)                                                     \
    do {                                                                                           \
        ArrayOfView<TYPE> data(condition, ArrayOfViewAssumeValidated {});                          \
        for (indexType k = 0; k < count; ++k) {                                                    \
            if (data[k] == 0) {                                                                    \
                return false;                                                                      \
            }                                                                                      \
        }                                                                                          \
        return true;                                                                               \
    } while (false)

    switch (condition.dataClass()) {
    case NLS_LOGICAL:
        ASSERT_ALL_NON_ZERO(logical, NLS_LOGICAL);
    case NLS_INT8:
        ASSERT_ALL_NON_ZERO(int8, NLS_INT8);
    case NLS_INT16:
        ASSERT_ALL_NON_ZERO(int16, NLS_INT16);
    case NLS_INT32:
        ASSERT_ALL_NON_ZERO(int32, NLS_INT32);
    case NLS_INT64:
        ASSERT_ALL_NON_ZERO(int64, NLS_INT64);
    case NLS_UINT8:
        ASSERT_ALL_NON_ZERO(uint8, NLS_UINT8);
    case NLS_UINT16:
        ASSERT_ALL_NON_ZERO(uint16, NLS_UINT16);
    case NLS_UINT32:
        ASSERT_ALL_NON_ZERO(uint32, NLS_UINT32);
    case NLS_UINT64:
        ASSERT_ALL_NON_ZERO(uint64, NLS_UINT64);
    case NLS_SINGLE:
        ASSERT_ALL_NON_ZERO(single, NLS_SINGLE);
    case NLS_DOUBLE:
        ASSERT_ALL_NON_ZERO(double, NLS_DOUBLE);
    default:
        return true;
    }

#undef ASSERT_ALL_NON_ZERO
}
//=============================================================================
static std::wstring
formatAssertMessage(
    Evaluator* eval, const std::wstring& format, const ArrayOfVector& args, size_t firstArg)
{
    ArrayOfVector printfArgs;
    printfArgs << ArrayOf::characterArray(format);
    for (size_t k = firstArg; k < args.size(); ++k) {
        printfArgs << args[k];
    }
    std::wstring result;
    std::wstring errorMessage;
    if (!StringPrintf(result, errorMessage, eval, printfArgs)) {
        RaiseError(errorMessage, L"Nelson:asserts:invalidFormat");
    }
    return result;
}
//=============================================================================
static void
parseAssertMessage(
    Evaluator* eval, const ArrayOfVector& argIn, std::wstring& identifier, std::wstring& message)
{
    message
        = resolveMessageTemplate(L"Nelson:assert_functions:assertionFailed", L"Assertion failed.");
    if (argIn.size() == 1) {
        return;
    }
    if (!isMessageArgument(argIn[1])) {
        Error(L"Nelson:asserts:invalidArgument:messageOrIdentifier",
            L"Assertion message or identifier must be a string.");
    }
    std::wstring second = argIn[1].asWideString();
    size_t messageIndex = 1;
    if (argIn.size() >= 3 && isIdentifier(second)) {
        identifier = second;
        messageIndex = 2;
        if (!isMessageArgument(argIn[2])) {
            Error(
                L"Nelson:asserts:invalidArgument:message", L"Assertion message must be a string.");
        }
    }
    message = argIn[messageIndex].asWideString();
    if (argIn.size() > messageIndex + 1) {
        message = formatAssertMessage(eval, message, argIn, messageIndex + 1);
    }
}
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assertBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = assertConditionPasses(argIn[0]);
    // Identifier and message are parsed only on failure; a passing assert(cond, ...)
    // ignores the extra arguments.
    std::wstring identifier;
    std::wstring message;
    if (!passed) {
        parseAssertMessage(eval, argIn, identifier, message);
    }
    if (nLhs == 0) {
        if (!passed) {
            RaiseError(message,
                identifier.empty() ? std::wstring(L"Nelson:asserts:assert") : identifier, true);
        }
        return {};
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(passed);
    if (nLhs > 1) {
        retval << ArrayOf::characterArray(passed ? L"" : message);
    }
    (void)eval;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_istrueBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(2);
    std::wstring modifiedmsg;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        modifiedmsg = param2.asWideString();
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isLogical()) {
        Error(L"Nelson:asserts:invalidArgument", L"Wrong type for argument #%d: %s expected.", (1),
            L"logical");
    }
    if (!param1.isScalar()) {
        Error(L"Nelson:asserts:scalarExpected", L"A scalar expected.");
    }
    logical res = param1.asLogicalScalar();
    std::wstring msg;
    res = Assert_IsTrue(res, modifiedmsg, msg);
    if (nLhs == 0) {
        if (res == 0) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", msg);
        }
    } else {
        retval << ArrayOf::logicalScalar(res == 0 ? false : true);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(msg);
        }
    }
    return retval;
}
//=============================================================================
