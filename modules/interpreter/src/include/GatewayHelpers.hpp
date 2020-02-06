//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
using PROC_InitializeGateway = bool (*)(Nelson::Evaluator*);
using PROC_FinishGateway = bool (*)(Nelson::Evaluator*);
//=============================================================================
extern "C"
{
    NLSINTERPRETER_IMPEXP
    int
    NelsonAddGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
        int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidInitializeFunction);
    //=============================================================================
    NLSINTERPRETER_IMPEXP int
    NelsonRemoveGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
        int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidFinishFunction);
}
//=============================================================================
