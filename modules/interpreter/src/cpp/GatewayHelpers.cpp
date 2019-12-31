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
#include "GatewayHelpers.hpp"
#include "Evaluator.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
int
NelsonAddGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
    int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidInitializeFunction)
{
    PROC_InitializeGateway ptrInitializeFunction
        = (PROC_InitializeGateway)ptrVoidInitializeFunction;
    const Nelson::nlsGateway* _gateway = (Nelson::nlsGateway*)gateway;
    Nelson::Evaluator* _eval = (Nelson::Evaluator*)eval;
    Context* ctx = _eval->getContext();
    if (ctx) {
        for (size_t k = 0; k < nbBuiltins; k++) {
            Nelson::BuiltInFunctionDefManager::getInstance()->add(_gateway[k].functionName.c_str(),
                _gateway[k].fptr, _gateway[k].nRhs, _gateway[k].nLhs, moduleFilename, gatewayName);
        }
        if ((void*)ptrInitializeFunction) {
            PROC_InitializeGateway ptrFunc
                = reinterpret_cast<PROC_InitializeGateway>(ptrInitializeFunction);
            return ptrFunc(_eval) ? 1 : 0;
        }
        return 1;
    }
    return 0;
}
//=============================================================================
int
NelsonRemoveGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
    int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidFinishFunction)
{
    PROC_FinishGateway ptrFinishFunction = (PROC_FinishGateway)ptrVoidFinishFunction;
    const Nelson::nlsGateway* _gateway = (Nelson::nlsGateway*)gateway;
    Nelson::Evaluator* _eval = (Nelson::Evaluator*)eval;
    Context* ctx = _eval->getContext();
    if (ctx) {
        for (size_t k = 0; k < nbBuiltins; ++k) {
            Nelson::BuiltInFunctionDefManager::getInstance()->remove(_gateway[k].fptr);
        }
        if ((void*)ptrFinishFunction) {
            PROC_FinishGateway ptrFunc = reinterpret_cast<PROC_FinishGateway>(ptrFinishFunction);
            return ptrFunc(_eval) ? 1 : 0;
        }
        return 1;
    }
    return 0;
}
//=============================================================================
