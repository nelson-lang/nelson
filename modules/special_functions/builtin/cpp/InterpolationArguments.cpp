//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "InterpolationArguments.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isInterpolationText(const ArrayOf& value)
{
    return value.isCharacterArray() || (value.isStringArray() && value.isScalar());
}
//=============================================================================
bool
readInterpolationText(const ArrayOf& value, std::wstring& text)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray(false)) {
        text = value.asWideString();
        return true;
    }
    return false;
}
//=============================================================================
bool
parseGriddedInterpolationTail(const ArrayOfVector& argIn, size_t& count, std::wstring& method,
    bool& useConstant, ArrayOf& extrapolationValue)
{
    useConstant = false;
    if (count >= 2 && argIn[count - 1].isNumeric() && argIn[count - 1].isScalar()
        && isInterpolationText(argIn[count - 2])) {
        useConstant = true;
        extrapolationValue = argIn[count - 1];
        count--;
    }
    method = L"linear";
    if (count >= 1 && isInterpolationText(argIn[count - 1])) {
        if (!readInterpolationText(argIn[count - 1], method)) {
            return false;
        }
        if (method == L"v5cubic") {
            method = L"cubic";
        }
        if (method != L"linear" && method != L"nearest" && method != L"cubic" && method != L"makima"
            && method != L"spline") {
            return false;
        }
        count--;
    }
    return true;
}
//=============================================================================
ArrayOfVector
callInterpolationFunction(
    Evaluator* eval, const std::string& name, const ArrayOfVector& args, int nLhs)
{
    FunctionDef* funcDef = nullptr;
    if (!eval->getContext()->lookupFunction(name, funcDef) || funcDef == nullptr) {
        Error(L"Nelson:interp:functionNotFound", L"Function not found.");
    }
    return funcDef->evaluateFunction(eval, args, nLhs);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
