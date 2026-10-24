//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ValidatorsInternal.hpp"
#include "ValidatorsHelpers.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isOfClass(const ArrayOf& arg, const std::wstring& currentClassName, const std::wstring& className);
static std::wstring
allowedClassesMessage(const wstringVector& classNames);
//=============================================================================
void
mustBeA(const ArrayOf& arg, const wstringVector& classNames, int argPosition, bool asCaller)
{
    std::wstring currentClassName;
    ClassName(arg, currentClassName);
    for (const auto& className : classNames) {
        if (isOfClass(arg, currentClassName, className)) {
            return;
        }
    }
    std::wstring msg = invalidPositionMessage(argPosition)
        + formatI18nMessage(resolveMessageTemplate(
                                L"Nelson:validators:mustBeA", L"%s must have one of these types:"),
            L"Value")
        + L" " + allowedClassesMessage(classNames);
    RaiseError(msg, L"Nelson:validators:mustBeA", asCaller);
}
//=============================================================================
bool
isOfClass(const ArrayOf& arg, const std::wstring& currentClassName, const std::wstring& className)
{
    // The exact class is the common case: isa is only evaluated for a
    // superclass or a category (numeric, float, integer).
    if (currentClassName == className) {
        return true;
    }
    ArrayOfVector argIn;
    argIn << arg;
    argIn << ArrayOf::characterArray(className);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isa");
    return argOut[0].asLogicalScalar();
}
//=============================================================================
std::wstring
allowedClassesMessage(const wstringVector& classNames)
{
    std::wstring concatClass;
    for (size_t k = 0; k < classNames.size(); ++k) {
        if (k == 0) {
            concatClass = L"'" + classNames[k] + L"'";
        } else if (k == classNames.size() - 1) {
            concatClass = concatClass + L", " + _W("or") + L" " + L"'" + classNames[k] + L"'";
        } else {
            concatClass = concatClass + L", " + L"'" + classNames[k] + L"'";
        }
        if (k == classNames.size() - 1) {
            concatClass = concatClass + L".";
        }
    }
    return concatClass;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
