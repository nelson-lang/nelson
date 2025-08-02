//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SaveMatioHandle.hpp"
#include "SaveMatioStruct.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
matvar_t*
SaveMatioHandle(const std::string& variableName, const ArrayOf& variableValue, mat_ft matVersion)
{
    /* handle have no equivalent in others softwares */
    Warning(_W("handle not saved."));
    return SaveMatioStruct(variableName, ArrayOf::emptyStructWithoutFields(), matVersion);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
