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
#include "nlsSpecial_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class BesselFunctionKind
{
    J,
    Y,
    I,
    K,
    H1,
    H2
};
//=============================================================================
// Bessel function of order NU at Z, element-wise with scalar expansion.
NLSSPECIAL_FUNCTIONS_IMPEXP ArrayOf
BesselFunction(BesselFunctionKind kind, const ArrayOf& nu, const ArrayOf& z, bool scaled);
//=============================================================================
// Airy function Ai (k = 0), Ai' (k = 1), Bi (k = 2) or Bi' (k = 3) at Z.
NLSSPECIAL_FUNCTIONS_IMPEXP ArrayOf
AiryFunction(int k, const ArrayOf& z, bool scaled);
//=============================================================================
// Checks the NU and Z arguments shared by the Bessel functions.
NLSSPECIAL_FUNCTIONS_IMPEXP void
checkBesselArguments(const ArrayOf& nu, const ArrayOf& z);
//=============================================================================
// Reads a 0 or 1 SCALE argument.
NLSSPECIAL_FUNCTIONS_IMPEXP bool
getBesselScaleArgument(const ArrayOf& scale);
//=============================================================================
// Reads an integer selector K in [low, high], raising errorId otherwise.
NLSSPECIAL_FUNCTIONS_IMPEXP int
getBesselOrderSelector(const ArrayOf& k, int low, int high, const std::wstring& errorId,
    const std::wstring& errorMessage);
//=============================================================================
// Checks that an argument is a single or double array.
NLSSPECIAL_FUNCTIONS_IMPEXP void
checkBesselFloatArgument(const ArrayOf& arg);
//=============================================================================
} // namespace Nelson
//=============================================================================
