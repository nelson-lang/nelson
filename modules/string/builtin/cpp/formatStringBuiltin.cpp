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
formatToWideStringImpl(NelsonType destinationType, const std::wstring& fmt,
    const std::vector<std::wstring>& args, size_t& replacedCount, size_t& usedCount);
//=============================================================================
ArrayOfVector
Nelson::StringGateway::formatStringBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    bool isFirstArgValid = argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray();
    if (!isFirstArgValid) {
        raiseError(L"Nelson:string:ERROR_FIRST_ARGUMENT_MUST_BE_CHARACTER_ARRAY_OR_STRING",
            ERROR_FIRST_ARGUMENT_MUST_BE_CHARACTER_ARRAY_OR_STRING);
    }
    ArrayOfVector retval;
    std::wstring formatStr = argIn[0].getContentAsWideString();
    std::vector<std::wstring> args;

    auto getArgAsWstring = [&](const ArrayOf& a) -> std::wstring {
        std::wstring res;
        switch (a.getDataClass()) {
        case NLS_LOGICAL: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = (a.getContentAsLogicalScalar() != 0) ? L"true" : L"false";
        } break;
        case NLS_INT8: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsInteger8Scalar());
        } break;
        case NLS_UINT8: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger8Scalar());
        } break;
        case NLS_INT16: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsInteger16Scalar());
        } break;
        case NLS_UINT16: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger16Scalar());
        } break;
        case NLS_INT32: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsInteger32Scalar());
        } break;
        case NLS_UINT32: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger32Scalar());
        } break;
        case NLS_INT64: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsInteger64Scalar());
        } break;
        case NLS_UINT64: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
            }
            res = std::to_wstring(a.getContentAsUnsignedInteger64Scalar());
        } break;
        case NLS_SINGLE: {
            if (a.numel() != 1) {
                raiseError2(L"nelson:validators:mustBeScalar");
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
                raiseError2(L"nelson:validators:mustBeScalar");
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
                raiseError(L"Nelson:string:ERROR_CHARACTER_ARGUMENT_MUST_BE_ROW_VECTOR",
                    ERROR_CHARACTER_ARGUMENT_MUST_BE_ROW_VECTOR);
            }
            res = a.getContentAsWideString();
        } break;
        case NLS_STRING_ARRAY: {
            if (!a.isScalarStringArray()) {
                raiseError2(L"nelson:validators:mustBeScalar");
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
            raiseError(L"Nelson:string:ERROR_UNSUPPORTED_ARGUMENT_TYPE_FOR_FORMATSTRING",
                ERROR_UNSUPPORTED_ARGUMENT_TYPE_FOR_FORMATSTRING);
        } break;
        }
        return res;
    };

    for (size_t i = 1; i < argIn.size(); ++i) {
        args.push_back(getArgAsWstring(argIn[i]));
    }

    size_t replacedCount = 0;
    size_t usedCount = 0;
    retval << formatToWideStringImpl(
        argIn[0].getDataClass(), formatStr, args, replacedCount, usedCount);
    if (usedCount != args.size()) {
        raiseError(L"nelson:runtime:incorrectHoleCount", formatStr);
    }
    return retval;
}
//=============================================================================
static ArrayOf
formatToWideStringImpl(NelsonType destinationType, const std::wstring& fmt,
    const std::vector<std::wstring>& args, size_t& replacedCount, size_t& usedCount)
{
    std::wstring out;
    out.reserve(fmt.size());
    size_t argIndex = 0;
    replacedCount = 0;
    usedCount = 0;
    std::vector<bool> used(args.size(), false);
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
                    raiseError(
                        L"Nelson:string:ERROR_INVALID_FORMAT_STRING", ERROR_INVALID_FORMAT_STRING);
                }
                std::wstring inner = fmt.substr(i + 1, j - (i + 1));
                if (inner.empty()) {
                    if (argIndex >= args.size()) {
                        raiseError(L"Nelson:string:ERROR_NOT_ENOUGH_ARGUMENTS_FOR_FORMAT_STRING",
                            ERROR_NOT_ENOUGH_ARGUMENTS_FOR_FORMAT_STRING);
                    }
                    out += args[argIndex++];
                    ++replacedCount;
                    used[argIndex - 1] = true;
                } else {
                    // support numeric index inside braces (0-based)
                    bool allDigits = std::all_of(inner.begin(), inner.end(),
                        [](wchar_t ch) { return std::iswdigit(ch) != 0; });
                    if (allDigits) {
                        unsigned long idx = 0;
                        try {
                            idx = std::stoul(inner);
                        } catch (...) {
                            raiseError(L"Nelson:string:ERROR_INVALID_FORMAT_STRING",
                                ERROR_INVALID_FORMAT_STRING);
                        }
                        if (idx >= args.size()) {
                            raiseError(L"Nelson:string:ERROR_WRONG_FORMAT_ARGUMENT_INDEX",
                                ERROR_WRONG_FORMAT_ARGUMENT_INDEX);
                        }
                        out += args[idx];
                        ++replacedCount;
                        used[idx] = true;
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
                raiseError(
                    L"Nelson:string:ERROR_INVALID_FORMAT_STRING", ERROR_INVALID_FORMAT_STRING);
            }
        } else {
            out.push_back(c);
            ++i;
        }
    }
    if (!used.empty()) {
        usedCount = static_cast<size_t>(std::count(used.begin(), used.end(), true));
    }
    if (destinationType == NLS_CHAR) {
        return ArrayOf::characterArrayConstructor(out);
    }
    return ArrayOf::stringArrayConstructor(out);
}
//=============================================================================
