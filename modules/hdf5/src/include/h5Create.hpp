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
#include <vector>
#include "nlsHdf5_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSHDF5_IMPEXP void
h5Create(const std::wstring& filename, const std::wstring& dataSetName,
    const std::vector<double>& sizeData, NelsonType dataType, const std::vector<double>& chunksize,
    int deflate, const ArrayOf& fillvalue, bool fletcher32 = false, bool shuffle = false,
    const std::wstring& textEncoding = L"system");
//=============================================================================
} // namespace Nelson
//=============================================================================
