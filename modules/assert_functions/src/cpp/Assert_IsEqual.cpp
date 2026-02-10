//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Assert_IsEqual.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
formatMessage(const ArrayOf& computedArray, const ArrayOf& expectedArray)
{
    std::wstring message;
    bool doDefaultMessage = false;
    if ((computedArray.isCharacterArray()
            && (computedArray.isRowVector() || computedArray.isScalar() || computedArray.isEmpty()))
        && (expectedArray.isCharacterArray()
            && (expectedArray.isRowVector() || expectedArray.isScalar()
                || expectedArray.isEmpty()))) {
        std::wstring computed = computedArray.getContentAsWideString();
        std::wstring expected = expectedArray.getContentAsWideString();
        message = fmt::format(
            _W("Assertion failed: expected '{0}' and computed '{1}' values are different."),
            expected, computed);
    } else {
        if (computedArray.isScalar() && expectedArray.isScalar()) {
            if ((computedArray.getDataClass() == expectedArray.getDataClass())
                && (!computedArray.isSparse() && !expectedArray.isSparse())) {
                switch (expectedArray.getDataClass()) {
                case NLS_LOGICAL: {
                    logical computed = computedArray.getContentAsLogicalScalar();
                    logical expected = expectedArray.getContentAsLogicalScalar();
                    message = fmt::format(_W("Assertion failed: expected ({0}) and computed ({1}) "
                                             "values are different."),
                        expected ? L"true" : L"false", computed ? L"true" : L"false");
                } break;
                case NLS_STRING_ARRAY: {
                    std::wstring computed = computedArray.getContentAsWideString();
                    std::wstring expected = expectedArray.getContentAsWideString();
                    message
                        = fmt::format(_W("Assertion failed: expected \"{0}\" and computed \"{1}\" "
                                         "values are different."),
                            expected, computed);

                } break;
                case NLS_DOUBLE: {
                    double computed = ArrayOf(computedArray).getContentAsDoubleScalar();
                    double expected = ArrayOf(expectedArray).getContentAsDoubleScalar();
                    message = fmt::format(_W("Assertion failed: expected ({0}) and computed ({1}) "
                                             "values are different."),
                        expected, computed);
                } break;
                case NLS_INT32: {
                    int32 computed = ArrayOf(computedArray).getContentAsInteger32Scalar();
                    int32 expected = ArrayOf(expectedArray).getContentAsInteger32Scalar();
                    message = fmt::format(_W("Assertion failed: expected ({0}) and computed ({1}) "
                                             "values are different."),
                        fmt::to_wstring(expected), fmt::to_wstring(computed));
                } break;
                case NLS_UINT64: {
                    uint64 computed = ArrayOf(computedArray).getContentAsUnsignedInteger64Scalar();
                    uint64 expected = ArrayOf(expectedArray).getContentAsUnsignedInteger64Scalar();
                    message = fmt::format(_W("Assertion failed: expected ({0}) and computed ({1}) "
                                             "values are different."),
                        fmt::to_wstring(expected), fmt::to_wstring(computed));
                } break;
                default: {
                    doDefaultMessage = true;
                } break;
                }
            } else {
                doDefaultMessage = true;
            }
        } else {
            doDefaultMessage = true;
        }
    }

    if (doDefaultMessage) {
        message = _W("Assertion failed: expected and computed values are different.");
    }
    return message;
}
//=============================================================================
bool
Assert_IsEqual(
    Evaluator* eval, const ArrayOf& computedArray, const ArrayOf& expectedArray, std::wstring& msg)
{
    bool bRes = false;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string IsEqualToName = "isequalto";
    if (context->lookupFunction(IsEqualToName, funcDef)) {
        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
            ArrayOfVector argInCopy(2);
            argInCopy << computedArray;
            argInCopy << expectedArray;
            try {
                ArrayOfVector resVect = funcDef->evaluateFunction(eval, argInCopy, 1);
                if (resVect.size() != 1) {
                    raiseError(
                        L"Nelson:assert_functions:ERROR_ISEQUALTO_RETURNS_MORE_THAN_ONE_OUTPUT_ARG",
                        ERROR_ISEQUALTO_RETURNS_MORE_THAN_ONE_OUTPUT_ARG);
                }
                ArrayOf r = resVect[0];
                if (r.isScalar() && r.isLogical()) {
                    bRes = r.getContentAsLogicalScalar() ? true : false;
                } else {
                    raiseError(L"Nelson:assert_functions:ERROR_ISEQUALTO_MUST_RETURN_LOGICAL",
                        ERROR_ISEQUALTO_MUST_RETURN_LOGICAL);
                }
            } catch (const Exception&) {
                raiseError(L"Nelson:assert_functions:ERROR_ISEQUALTO_RETURNS_UNEXPECTED_ERROR",
                    ERROR_ISEQUALTO_RETURNS_UNEXPECTED_ERROR);
            }
        }
    } else {
        raiseError(L"Nelson:assert_functions:ERROR_ISEQUALTO_FUNCTION_NOT_FOUND",
            ERROR_ISEQUALTO_FUNCTION_NOT_FOUND);
    }
    if (!bRes) {
        msg = formatMessage(computedArray, expectedArray);
    } else {
        msg.clear();
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
