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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsRandom_exports.h"
//=============================================================================
namespace Nelson {
NLSRANDOM_IMPEXP std::wstring
RngGetType(Evaluator* eval);
NLSRANDOM_IMPEXP void
RngSetSeed(Evaluator* eval, double seed);
NLSRANDOM_IMPEXP ArrayOf
RngGetSeed(Evaluator* eval);
NLSRANDOM_IMPEXP ArrayOf
RngGetState(Evaluator* eval);
NLSRANDOM_IMPEXP ArrayOf
RngGetState(Evaluator* eval);
NLSRANDOM_IMPEXP void
RngSetDefault(Evaluator* eval);
NLSRANDOM_IMPEXP void
RngShuffle(Evaluator* eval);
NLSRANDOM_IMPEXP bool
RngSetEngine(Evaluator* eval, double seed, std::wstring engineName);
NLSRANDOM_IMPEXP void
RngDelete(Evaluator* eval);
NLSRANDOM_IMPEXP bool
RngSetState(Evaluator* eval, ArrayOf st);
} // namespace Nelson
//=============================================================================
