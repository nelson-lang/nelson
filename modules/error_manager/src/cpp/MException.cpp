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
isIdentifierPart(wchar_t c)
{
    return (iswalpha(c) || iswdigit(c) || c == L'_');
}
//=============================================================================
bool
isValidMExceptionIdentifier(std::wstring identifier)
{
    wstringVector splitComponents;
    StringHelpers::split(splitComponents, identifier, L':');
    if (splitComponents.size() < 2) {
        return false;
    }
    for (std::wstring component : splitComponents) {
        if (component.empty()) {
            return false;
        }
        if (!iswalpha(component[0])) {
            return false;
        }
        if (component.size() > 1) {
            component.erase(0, 1);
            if (find_if_not(component.begin(), component.end(), isIdentifierPart)
                != component.end()) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
