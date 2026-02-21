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
    bool doDefaultMessage = true;

    auto scalarToWString = [](const ArrayOf& a) -> std::optional<std::wstring> {
        try {
            switch (a.getDataClass()) {
            case NLS_LOGICAL: {
                logical v = a.getContentAsLogicalScalar();
                return std::optional<std::wstring>(v ? TRUE_STR : FALSE_STR);
            }
            case NLS_DOUBLE: {
                double v = a.getContentAsDoubleScalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_SINGLE: {
                float v = a.getContentAsSingleScalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_INT8: {
                int8 v = a.getContentAsInteger8Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_UINT8: {
                uint8 v = a.getContentAsUnsignedInteger8Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_INT16: {
                int16 v = a.getContentAsInteger16Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_UINT16: {
                uint16 v = a.getContentAsUnsignedInteger16Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_INT32: {
                int32 v = a.getContentAsInteger32Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_UINT32: {
                uint32 v = a.getContentAsUnsignedInteger32Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_INT64: {
                int64 v = a.getContentAsInteger64Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_UINT64: {
                uint64 v = a.getContentAsUnsignedInteger64Scalar();
                return std::optional<std::wstring>(fmt::format(L"{}", v));
            }
            case NLS_DCOMPLEX: {
                std::complex<double> v = a.getContentAsDoubleComplexScalar();
                if (v.imag() == 0) {
                    return std::optional<std::wstring>(fmt::format(L"{}", v.real()));
                }
                if (v.real() == 0) {
                    return std::optional<std::wstring>(fmt::format(L"{}i", v.imag()));
                }
                if (v.imag() < 0) {
                    return std::optional<std::wstring>(
                        fmt::format(L"{} - {}i", v.real(), -v.imag()));
                }
                return std::optional<std::wstring>(fmt::format(L"{} + {}i", v.real(), v.imag()));
            }
            case NLS_SCOMPLEX: {
                std::complex<single> v = a.getContentAsSingleComplexScalar();
                if (v.imag() == 0) {
                    return std::optional<std::wstring>(fmt::format(L"{}", v.real()));
                }
                if (v.real() == 0) {
                    return std::optional<std::wstring>(fmt::format(L"{}i", v.imag()));
                }
                if (v.imag() < 0) {
                    return std::optional<std::wstring>(
                        fmt::format(L"{} - {}i", v.real(), -v.imag()));
                }
            }
            case NLS_CHAR: {
                if (a.isRowVector() || a.isEmpty()) {
                    std::wstring v = a.getContentAsWideString();
                    return std::optional<std::wstring>(fmt::format(L"{}", v));
                }
            }
            case NLS_STRING_ARRAY: {
                if (a.isScalar()) {
                    std::wstring v = a.getContentAsWideString();
                    return std::optional<std::wstring>(fmt::format(L"{}", v));
                }
            }
            default:
                return std::nullopt;
            }
        } catch (...) {
            return std::nullopt;
        }
    };
    if ((computedArray.getDataClass() == expectedArray.getDataClass())
        && (computedArray.isScalar() && expectedArray.isScalar()) && !computedArray.isSparse()
        && !expectedArray.isSparse()) {
        // Try to convert both scalars to strings using the helper
        auto sComputed = scalarToWString(computedArray);
        auto sExpected = scalarToWString(expectedArray);
        if (sComputed && sExpected) {
            message = formatErrorMessage(
                _E("nelson:assert:assertionFailedValueExpectedComputed"), *sExpected, *sComputed);
            doDefaultMessage = false;
        } else {
            doDefaultMessage = true;
        }
    }

    if (doDefaultMessage) {
        message = formatErrorMessage(_E("nelson:assert:assertionFailedValuesDifferent"));
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
