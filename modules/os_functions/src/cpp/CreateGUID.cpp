//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "CreateGUID.hpp"
#include "UuidHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
CreateGUID()
{
    std::wstring result;
    UuidHelpers::generateUuid(result);
    return result;
}
//=============================================================================
wstringVector
CreateGUID(size_t nbGUID)
{
    wstringVector uuids;
    for (size_t k = 0; k < nbGUID; k++) {
        uuids.push_back(CreateGUID());
    }
    return uuids;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
