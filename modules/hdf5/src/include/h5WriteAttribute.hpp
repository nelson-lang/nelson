//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "nlsHdf5_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSHDF5_IMPEXP void
h5WriteAttribute(const std::wstring& filename, const std::wstring& location,
    const std::wstring& attributeName, ArrayOf& attributeValue,
    const std::wstring& textEncoding = L"system");
//=============================================================================
}
//=============================================================================
