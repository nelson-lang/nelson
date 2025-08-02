//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LoadMatioOpaque.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioOpaque(matvar_t* matVariable, ArrayOf& VariableValue)
{
    // Not managed by matio 1.5.13
    if (matVariable != nullptr) {
        Warning(WARNING_MATIO_TYPE_NOT_SUPPORTED,
            _W("Cannot read variable:") + L" " + utf8_to_wstring(matVariable->name));
    } else {
        Warning(WARNING_MATIO_TYPE_NOT_SUPPORTED, _W("Cannot read variable."));
    }
    VariableValue = ArrayOf::emptyStructWithoutFields();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
