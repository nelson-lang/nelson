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
enum FftPlannerMethod
{
    UNKNOWN = -1,
    ESTIMATE,
    MEASURE,
    PATIENT,
    EXHAUSTIVE,
    HYBRID
};
//=============================================================================
indexType
computeDim(const ArrayOf& X);
//=============================================================================
ArrayOf
scomplexFFTW(const ArrayOf& X, indexType n, indexType dim, bool asInverse);
//=============================================================================
ArrayOf
dcomplexFFTW(const ArrayOf& X, indexType n, indexType dim, bool asInverse);
//=============================================================================
NLSFFTW_IMPEXP std::wstring
getDoubleWisdomInformation();
//=============================================================================
NLSFFTW_IMPEXP std::wstring
getSingleWisdomInformation();
//=============================================================================
NLSFFTW_IMPEXP std::wstring
getPlannerInformation();
//=============================================================================
NLSFFTW_IMPEXP bool
setDoubleWisdomInformation(const std::wstring& info);
//=============================================================================
NLSFFTW_IMPEXP bool
setSingleWisdomInformation(const std::wstring& info);
//=============================================================================
NLSFFTW_IMPEXP bool
setPlannerInformation(FftPlannerMethod newMethod);
//=============================================================================
NLSFFTW_IMPEXP void
resetDoubleWisdom();
//=============================================================================
NLSFFTW_IMPEXP void
resetSingleWisdom();
//=============================================================================
NLSFFTW_IMPEXP void
resetPlanner();
//=============================================================================
} // namespace Nelson
//=============================================================================
