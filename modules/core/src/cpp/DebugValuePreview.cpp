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
#include <cmath>
#include <complex>
#include <iomanip>
#include <sstream>
#include <regex>
#include "DebugValuePreview.hpp"
#include "ClassToString.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEBUG_PREVIEW_TEMP_VARIABLE "__nelson_dbtip__"
static const size_t MAX_PREVIEW_ELEMENTS = 10;
//=============================================================================
static std::wstring
formatReal(double value)
{
    if (std::isnan(value)) {
        return L"NaN";
    }
    if (std::isinf(value)) {
        return value > 0 ? L"Inf" : L"-Inf";
    }
    if (value == std::floor(value) && std::fabs(value) < 1e15) {
        std::wostringstream stream;
        stream << static_cast<long long>(value);
        return stream.str();
    }
    std::wostringstream stream;
    stream << std::setprecision(5) << value;
    return stream.str();
}
//=============================================================================
template <typename T>
static std::wstring
numericElementText(const ArrayOf& value, size_t index)
{
    const T* data = static_cast<const T*>(value.dataPointer());
    return formatReal(static_cast<double>(data[index]));
}
//=============================================================================
template <typename T>
static std::wstring
complexElementText(const ArrayOf& value, size_t index)
{
    const T* data = static_cast<const T*>(value.dataPointer());
    T real = data[2 * index];
    T imag = data[2 * index + 1];
    std::wstring text = formatReal(static_cast<double>(real));
    text += imag < 0 ? L" - " : L" + ";
    text += formatReal(std::fabs(static_cast<double>(imag))) + L"i";
    return text;
}
//=============================================================================
static std::wstring
elementText(const ArrayOf& value, size_t index)
{
    switch (value.dataClass()) {
    case NLS_DOUBLE:
        return numericElementText<double>(value, index);
    case NLS_SINGLE:
        return numericElementText<single>(value, index);
    case NLS_DCOMPLEX:
        return complexElementText<double>(value, index);
    case NLS_SCOMPLEX:
        return complexElementText<single>(value, index);
    case NLS_INT8:
        return numericElementText<int8>(value, index);
    case NLS_INT16:
        return numericElementText<int16>(value, index);
    case NLS_INT32:
        return numericElementText<int32>(value, index);
    case NLS_INT64:
        return numericElementText<int64>(value, index);
    case NLS_UINT8:
        return numericElementText<uint8>(value, index);
    case NLS_UINT16:
        return numericElementText<uint16>(value, index);
    case NLS_UINT32:
        return numericElementText<uint32>(value, index);
    case NLS_UINT64:
        return numericElementText<uint64>(value, index);
    case NLS_LOGICAL: {
        const logical* data = static_cast<const logical*>(value.dataPointer());
        return data[index] != 0 ? L"true" : L"false";
    }
    default:
        return L"";
    }
}
//=============================================================================
static std::wstring
classSummary(const ArrayOf& value)
{
    NelsonType valueType = value.dataClass();
    std::wstring className = ClassToStringW(valueType);
    if (valueType == NLS_HANDLE) {
        className = utf8_to_wstring(value.handleCategory());
    }
    if (valueType == NLS_CLASS_ARRAY || valueType == NLS_CLASSDEF_ARRAY) {
        className = utf8_to_wstring(value.classType());
    }
    if (value.isSparse()) {
        className = L"sparse " + className;
    }
    return value.dimensions().toWideString() + L" " + className;
}
//=============================================================================
static std::wstring
truncatePreview(const std::wstring& text, size_t maxLength)
{
    if (maxLength == 0 || text.size() <= maxLength) {
        return text;
    }
    return text.substr(0, maxLength) + L"...";
}
//=============================================================================
std::wstring
variableValueSummary(const ArrayOf& value, size_t maxLength)
{
    std::wstring text;
    switch (value.dataClass()) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL: {
        if (value.isSparse()) {
            text = classSummary(value);
        } else if (value.isEmpty()) {
            text = L"[]";
        } else if (value.isScalar()) {
            text = elementText(value, 0);
        } else if (value.is2D() && value.elementCount() <= MAX_PREVIEW_ELEMENTS) {
            // small matrix shown row by row
            Dimensions dims = value.dimensions();
            size_t rows = dims[0];
            size_t columns = dims[1];
            text = L"[";
            for (size_t r = 0; r < rows; ++r) {
                if (r > 0) {
                    text += L"; ";
                }
                for (size_t c = 0; c < columns; ++c) {
                    if (c > 0) {
                        text += L" ";
                    }
                    text += elementText(value, r + c * rows);
                }
            }
            text += L"]";
        } else {
            text = classSummary(value);
        }
    } break;
    case NLS_CHAR: {
        if (value.isEmpty()) {
            text = L"''";
        } else if (value.isRowVector() || value.isScalar()) {
            text = L"'" + value.asWideString() + L"'";
        } else {
            text = classSummary(value);
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (value.isScalar()) {
            const ArrayOf* element = static_cast<const ArrayOf*>(value.dataPointer());
            if (element != nullptr && element->isDoubleClass()) {
                text = L"<missing>";
            } else {
                text = L"\"" + value.asWideString() + L"\"";
            }
        } else {
            text = classSummary(value);
        }
    } break;
    case NLS_CELL_ARRAY: {
        text = value.isEmpty() ? L"{}" : classSummary(value);
    } break;
    case NLS_STRUCT_ARRAY: {
        text = classSummary(value);
        stringVector names = value.fieldNames();
        if (!names.empty()) {
            text += L" (";
            for (size_t k = 0; k < names.size(); ++k) {
                if (k > 0) {
                    text += L", ";
                }
                text += utf8_to_wstring(names[k]);
            }
            text += L")";
        }
    } break;
    default: {
        text = classSummary(value);
    } break;
    }
    return truncatePreview(text, maxLength);
}
//=============================================================================
static bool
isDottedName(const std::wstring& text)
{
    static const std::wregex pattern(L"^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*$");
    return std::regex_match(text, pattern);
}
//=============================================================================
bool
debugVariablePreview(Evaluator* eval, const std::wstring& name, std::wstring& preview)
{
    preview.clear();
    if (eval == nullptr || eval->getContext() == nullptr || !isDottedName(name)) {
        return false;
    }
    std::vector<std::wstring> parts;
    size_t start = 0;
    while (true) {
        size_t dot = name.find(L'.', start);
        parts.push_back(name.substr(start, dot == std::wstring::npos ? dot : dot - start));
        if (dot == std::wstring::npos) {
            break;
        }
        start = dot + 1;
    }
    ArrayOf value;
    if (!eval->getContext()->lookupVariable(wstring_to_utf8(parts[0]), value)) {
        return false;
    }
    for (size_t k = 1; k < parts.size(); ++k) {
        // only struct fields are followed, without running any code
        if (!value.isStruct() || !value.isScalar()) {
            return false;
        }
        std::string fieldName = wstring_to_utf8(parts[k]);
        stringVector names = value.fieldNames();
        if (std::find(names.begin(), names.end(), fieldName) == names.end()) {
            return false;
        }
        ArrayOfVector fields = value.fieldAsList(fieldName);
        if (fields.empty()) {
            return false;
        }
        value = fields[0];
    }
    preview = name + L" = " + variableValueSummary(value);
    return true;
}
//=============================================================================
bool
debugExpressionPreview(Evaluator* eval, const std::wstring& expression, std::wstring& preview,
    std::wstring& errorMessage)
{
    preview.clear();
    errorMessage.clear();
    if (eval == nullptr || eval->getContext() == nullptr || expression.empty()) {
        return false;
    }
    if (!eval->isAtPrompt()) {
        return false;
    }
    Evaluator::DebugHooksSuppressor suppressor(eval);
    NelsonException savedLastError(eval->getLastErrorException());
    NelsonConfiguration* config = NelsonConfiguration::getInstance();
    bool interruptWasPending = config->getInterruptPending(eval->getID());
    const std::string temporaryName(DEBUG_PREVIEW_TEMP_VARIABLE);
    bool ok = false;
    try {
        eval->evaluateString(utf8_to_wstring(temporaryName) + L" = (" + expression + L");", true);
        ArrayOf value;
        if (eval->getContext()->lookupVariableLocally(temporaryName, value)) {
            preview = expression + L" = " + variableValueSummary(value);
            ok = true;
        }
    } catch (const NelsonException& e) {
        if (eval->getState() == NLS_STATE_ABORT || eval->isQuitOrForceQuitState()) {
            eval->getContext()->deleteVariable(temporaryName);
            throw;
        }
        errorMessage = e.message();
    }
    eval->getContext()->deleteVariable(temporaryName);
    eval->setLastErrorException(savedLastError);
    if (interruptWasPending) {
        config->setInterruptPending(true, eval->getID());
    }
    return ok;
}
//=============================================================================
bool
debugValuePreview(
    Evaluator* eval, const std::wstring& text, std::wstring& preview, std::wstring& errorMessage)
{
    preview.clear();
    errorMessage.clear();
    std::wstring trimmed = text;
    size_t first = trimmed.find_first_not_of(L" \t\r\n");
    size_t last = trimmed.find_last_not_of(L" \t\r\n");
    if (first == std::wstring::npos) {
        return false;
    }
    trimmed = trimmed.substr(first, last - first + 1);
    if (isDottedName(trimmed)) {
        return debugVariablePreview(eval, trimmed, preview);
    }
    return debugExpressionPreview(eval, trimmed, preview, errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
