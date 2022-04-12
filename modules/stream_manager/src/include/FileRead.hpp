//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Endian.hpp"
#include "File.hpp"
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSSTREAM_MANAGER_IMPEXP ArrayOf
FileRead(File* fp, int64 sizeToRead, NelsonType classPrecision, size_t skip, bool bIsLittleEndian,
    int& sizeReallyRead);
};
//=============================================================================
