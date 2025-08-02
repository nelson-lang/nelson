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
#include "ArrayOf.hpp"
#include "nlsFftw_exports.h"
//=============================================================================
namespace Nelson {
NLSFFTW_IMPEXP ArrayOf
InverseFft(const ArrayOf& X, indexType n, indexType dim);
NLSFFTW_IMPEXP ArrayOf
InverseFft(const ArrayOf& X, indexType n);
NLSFFTW_IMPEXP ArrayOf
InverseFft(const ArrayOf& X);
} // namespace Nelson
//=============================================================================
