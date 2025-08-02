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
#include "nlsElementary_mathematics_exports.h"
#include <cstdlib>
#ifndef M_PI
#define M_PI 3.141592653589793
#endif
//=============================================================================
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsInfinite(float t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsInfinite(double t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsNaN(int t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsNaN(unsigned int t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsNaN(double t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsFinite(float t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsFinite(double t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerForm(const double* t, size_t nbElements);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerForm(const float* t, size_t nbElements);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerForm(float t);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerForm(double t);

NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerFormOrNotFinite(const double* t, size_t nbElements);
NLSELEMENTARY_MATHEMATICS_IMPEXP bool
IsIntegerFormOrNotFinite(const float* t, size_t nbElements);
//=============================================================================
