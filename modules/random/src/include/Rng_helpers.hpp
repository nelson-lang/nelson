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
#include "nlsRandom_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum RNG_TYPE
{
    RNG_ERROR,
    RNG_TWISTER,
    RNG_TWISTER64,
    RNG_MRG32K3A,
    RNG_DSFMT19937,
    RNG_LAGGED_FIBONACCI_607,
    RNG_PHILOX,
    RNG_THREEFRY
};
//=============================================================================
NLSRANDOM_IMPEXP wstringVector
getSupportedRngEngineName();
//=============================================================================
NLSRANDOM_IMPEXP std::wstring
getRngTypeAsString(RNG_TYPE rngType);
//=============================================================================
NLSRANDOM_IMPEXP RNG_TYPE
getRngType(const std::wstring& enginename);
//=============================================================================
NLSRANDOM_IMPEXP bool
isRngType(const std::wstring& enginename);
//=============================================================================
NLSRANDOM_IMPEXP bool
haveRandomEngine();
//=============================================================================
} // namespace Nelson
//=============================================================================
