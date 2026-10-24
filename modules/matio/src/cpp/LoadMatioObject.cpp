//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LoadMatioObject.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LoadMatioObject(matvar_t* matVariable, ArrayOf& VariableValue)
{
    // Not managed by matio 1.5.13
    if (matVariable != nullptr) {
        RaiseWarning(WARNING_MATIO_TYPE_NOT_SUPPORTED,
            formatI18nMessage(resolveMessageTemplate(
                                  L"Nelson:hdf5:cannotReadVariableS", L"Cannot read variable: %s"),
                utf8_to_wstring(matVariable->name)));
    } else {
        Warning(L"Nelson:matio:type-not-supported", L"Cannot read variable.");
    }
    VariableValue = ArrayOf::emptyStruct();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
