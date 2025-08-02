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
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
NLSTIME_IMPEXP void
DateVector(double dateSerial, double& Y, double& M, double& D, double& H, double& MN, double& S,
    double& MS, bool rf);
}
//=============================================================================
