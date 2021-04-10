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
#include "NelsonGateway.hpp"
#include "ValidatorsInternal.hpp"
#include "mustBeLogicalScalarBuiltin.hpp"
#include "mustBeLogicalBuiltin.hpp"
#include "mustBeFiniteBuiltin.hpp"
#include "mustBeScalarOrEmptyBuiltin.hpp"
#include "mustBeValidVariableNameBuiltin.hpp"
#include "mustBeTextScalarBuiltin.hpp"
#include "mustBeFolderBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"validators";
//=============================================================================
static const nlsGateway gateway[] = {
    { "mustBeLogical", (void*)Nelson::ValidatorsGateway::mustBeLogicalBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeLogicalScalar", (void*)Nelson::ValidatorsGateway::mustBeLogicalScalarBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeFinite", (void*)Nelson::ValidatorsGateway::mustBeFiniteBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeScalarOrEmpty", (void*)Nelson::ValidatorsGateway::mustBeScalarOrEmptyBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeValidVariableName", (void*)Nelson::ValidatorsGateway::mustBeValidVariableNameBuiltin,
        0, -1, CPP_BUILTIN },
    { "mustBeTextScalar", (void*)Nelson::ValidatorsGateway::mustBeTextScalarBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeFolder", (void*)Nelson::ValidatorsGateway::mustBeFolderBuiltin, 0, -1, CPP_BUILTIN },

};
//=============================================================================
static bool
initializeValidatorsModule(Nelson::Evaluator* eval)
{
    setEvaluator(eval);
    return true;
}
//=============================================================================
static bool
finishValidatorsModule(Nelson::Evaluator* eval)
{
    return true;
}
//=============================================================================
NLSGATEWAYFUNCEXTENDED(gateway, (void*)initializeValidatorsModule)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishValidatorsModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
