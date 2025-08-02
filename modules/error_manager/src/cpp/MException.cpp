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
#include "StringHelpers.hpp"
#include "MException.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline bool
isAlphaNum(wchar_t c)
{
    return (iswalpha(c) || iswdigit(c));
}
//=============================================================================
bool
isValidMExceptionIdentifier(std::wstring identifier)
{
    wstringVector splittedComponents;
    StringHelpers::split(splittedComponents, identifier, L':');
    if (splittedComponents.size() < 2) {
        return false;
    }
    for (std::wstring component : splittedComponents) {
        if (component.empty()) {
            return false;
        }
        if (!iswalpha(component[0])) {
            return false;
        }
        if (component.size() > 1) {
            component.erase(0, 1);
            if (find_if(component.begin(), component.end(), isAlphaNum) == component.end()) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
}
//=============================================================================
