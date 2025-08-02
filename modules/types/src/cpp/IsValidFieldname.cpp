//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidFieldname.hpp"
#include "MakeValidFieldname.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsValidFieldname(const std::string& fieldname)
{
    std::string validFieldname = MakeValidFieldname(fieldname);
    return (validFieldname == fieldname);
}
//=============================================================================
}
//=============================================================================
