//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "formatStringBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOf.hpp"
#include <string>
#include <vector>
#include <cwctype>
#include <algorithm>
#include <sstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
formatToWideStringImpl(
    NelsonType destinationType, const std::wstring& fmt, const std::vector<std::wstring>& args);
//=============================================================================
ArrayOfVector
Nelson::StringGateway::formatStringBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    bool isFirstArgValid = argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray();
    if (!isFirstArgValid) {
        Error(_W("First argument must be a character array or string."));
    }
    ArrayOfVector retval;
    std::wstring formatStr = argIn[0].getContentAsWideString();
    std::vector<std::wstring> args;

    auto getArgAsWstring = [&](const ArrayOf& a) -> std::wstring {
        std::wstring res;
        switch (a.getDataClass()) {
        case NLS_LOGICAL: {
            if (a.numel() != 1) {
                Error(_W("Logical argument must be a scalar."), L"Nelson:LogicalArgumentExpected");
            }
            res = (a.getContentAsLogicalScalar() != 0) ? L"true" : L"false";
        } break;
        case NLS_INT8: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsInteger8Scalar());
        } break;
        case NLS_UINT8: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger8Scalar());
        } break;
        case NLS_INT16: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsInteger16Scalar());
        } break;
        case NLS_UINT16: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger16Scalar());
        } break;
        case NLS_INT32: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsInteger32Scalar());
        } break;
        case NLS_UINT32: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger32Scalar());
        } break;
        case NLS_INT64: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsInteger64Scalar());
        } break;
        case NLS_UINT64: {
            if (a.numel() != 1) {
                Error(_W("Integer argument must be a scalar."), L"Nelson:IntegerArgumentExpected");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger64Scalar());
        } break;
        case NLS_SINGLE: {
            if (a.numel() != 1) {
                Error(_W("Numeric argument must be a scalar."), L"Nelson:NumericArgumentExpected");
            }
            double dv = static_cast<double>(a.getContentAsSingleScalar());
            if (std::isfinite(dv) && std::floor(dv) == dv) {
                // integer value -> print without decimal part
                long long iv = static_cast<long long>(dv);
                res = std::to_wstring(iv);
            } else {
                std::wostringstream woss;
                woss.imbue(std::locale::classic());
                woss << dv;
                res = woss.str();
            }
        } break;
        case NLS_DOUBLE: {
            if (a.numel() != 1) {
                Error(_W("Numeric argument must be a scalar."), L"Nelson:NumericArgumentExpected");
            }
            double dv = a.getContentAsDoubleScalar();
            if (std::isfinite(dv) && std::floor(dv) == dv) {
                long long iv = static_cast<long long>(dv);
                res = std::to_wstring(iv);
            } else {
                std::wostringstream woss;
                woss.imbue(std::locale::classic());
                woss << dv;
                res = woss.str();
            }
        } break;
        case NLS_CHAR: {
            if (!a.isRowVectorCharacterArray()) {
                Error(_W("Character argument must be a row character vector."));
            }
            res = a.getContentAsWideString();
        } break;
        case NLS_STRING_ARRAY: {
            if (!a.isScalarStringArray()) {
                Error(_W("String argument must be a scalar string."));
            }
            res = a.getContentAsWideString();
        } break;
        case NLS_CELL_ARRAY:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_STRUCT_ARRAY:
        case NLS_HANDLE:
        case NLS_GO_HANDLE:
        default: {
            Error(_W("Unsupported argument type for formatString."));
        } break;
        }
        return res;
    };

    for (size_t i = 1; i < argIn.size(); ++i) {
        args.push_back(getArgAsWstring(argIn[i]));
    }

    retval << formatToWideStringImpl(argIn[0].getDataClass(), formatStr, args);

    return retval;
}
//=============================================================================
static ArrayOf
formatToWideStringImpl(
    NelsonType destinationType, const std::wstring& fmt, const std::vector<std::wstring>& args)
{
    std::wstring out;
    out.reserve(fmt.size());
    size_t argIndex = 0;
    for (size_t i = 0; i < fmt.size();) {
        wchar_t c = fmt[i];
        if (c == L'{') {
            if (i + 1 < fmt.size() && fmt[i + 1] == L'{') {
                out.push_back(L'{');
                i += 2;
            } else {
                // find closing '}'
                size_t j = fmt.find(L'}', i + 1);
                if (j == std::wstring::npos) {
                    Error(_W("Invalid format string."));
                }
                std::wstring inner = fmt.substr(i + 1, j - (i + 1));
                if (inner.empty()) {
                    if (argIndex >= args.size()) {
                        Error(_W("Not enough arguments for format string."));
                    }
                    out += args[argIndex++];
                } else {
                    // support numeric index inside braces (0-based)
                    bool allDigits = std::all_of(inner.begin(), inner.end(),
                        [](wchar_t ch) { return std::iswdigit(ch) != 0; });
                    if (allDigits) {
                        unsigned long idx = 0;
                        try {
                            idx = std::stoul(inner);
                        } catch (...) {
                            Error(_W("Invalid format string."));
                        }
                        if (idx >= args.size()) {
                            Error(_W("Wrong format argument index."));
                        }
                        out += args[idx];
                    } else {
                        // unknown format spec, keep it verbatim
                        out.append(L"{" + inner + L"}");
                    }
                }
                i = j + 1;
            }
        } else if (c == L'}') {
            if (i + 1 < fmt.size() && fmt[i + 1] == L'}') {
                out.push_back(L'}');
                i += 2;
            } else {
                Error(_W("Invalid format string."));
            }
        } else {
            out.push_back(c);
            ++i;
        }
    }
    if (destinationType == NLS_CHAR) {
        return ArrayOf::characterArrayConstructor(out);
    }
    return ArrayOf::stringArrayConstructor(out);
}
//=============================================================================
