//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mustBeBetweenBuiltin.hpp"
#include "ValidatorsInternal.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
#include <algorithm>
#include <cwctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::wstring
toLower(std::wstring value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return value;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar());
}
//=============================================================================
bool
isIntervalType(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        return false;
    }
    std::wstring text = toLower(value.getContentAsWideString());
    return text == L"closed" || text == L"open" || text == L"openleft" || text == L"openright"
        || text == L"closedleft" || text == L"closedright";
}
//=============================================================================
bool
hasValidMustBeBetweenShape(const ArrayOfVector& args)
{
    if (args.size() < 3) {
        return false;
    }
    size_t index = 3;
    if (args.size() > index && isIntervalType(args[index])) {
        index++;
    }
    return ((args.size() - index) % 2) == 0;
}
//=============================================================================
bool
isPositiveIntegerScalar(const ArrayOf& value, int& scalar)
{
    if (!value.isNumeric() || !value.isScalar()) {
        return false;
    }
    scalar = value.getContentAsInteger32Scalar();
    return scalar > 0;
}
//=============================================================================
ArrayOfVector
extractOptionalArgumentPosition(const ArrayOfVector& argIn, int& argPos)
{
    ArrayOfVector validatorArgs(argIn);
    argPos = -1;
    if (validatorArgs.size() <= 3) {
        return validatorArgs;
    }
    int candidatePosition = -1;
    if (isPositiveIntegerScalar(validatorArgs.back(), candidatePosition)) {
        ArrayOfVector candidateArgs(validatorArgs);
        candidateArgs.pop_back();
        if (hasValidMustBeBetweenShape(candidateArgs)) {
            validatorArgs = candidateArgs;
            argPos = candidatePosition;
        }
    }
    return validatorArgs;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeBetweenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3);
    int argPos = -1;
    ArrayOfVector validatorArgs = extractOptionalArgumentPosition(argIn, argPos);
    mustBeBetween(validatorArgs, argPos, true);
    return retval;
}
//=============================================================================
