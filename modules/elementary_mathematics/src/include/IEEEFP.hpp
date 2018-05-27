//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
//=============================================================================
