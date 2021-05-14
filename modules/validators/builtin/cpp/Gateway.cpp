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
#include "mustBeNonemptyBuiltin.hpp"
#include "mustBeScalarOrEmptyBuiltin.hpp"
#include "mustBeValidVariableNameBuiltin.hpp"
#include "mustBeTextScalarBuiltin.hpp"
#include "mustBeTextBuiltin.hpp"
#include "mustBeFolderBuiltin.hpp"
#include "mustBeFileBuiltin.hpp"
#include "mustBeVectorBuiltin.hpp"
#include "mustBeFloatBuiltin.hpp"
#include "mustBeNumericBuiltin.hpp"
#include "mustBeABuiltin.hpp"
#include "mustBePositiveBuiltin.hpp"
#include "mustBeNonnegativeBuiltin.hpp"
#include "mustBeNegativeBuiltin.hpp"
#include "mustBeNonpositiveBuiltin.hpp"
#include "mustBeNonNanBuiltin.hpp"
#include "mustBeNonZeroBuiltin.hpp"
#include "mustBeNonSparseBuiltin.hpp"
#include "mustBeRealBuiltin.hpp"
#include "mustBeIntegerBuiltin.hpp"
#include "mustBeNonmissingBuiltin.hpp"
#include "mustBeGreaterThanBuiltin.hpp"
#include "mustBeLessThanBuiltin.hpp"
#include "mustBeGreaterThanOrEqualBuiltin.hpp"
#include "mustBeLessThanOrEqualBuiltin.hpp"
#include "mustBeNumericOrLogicalBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"validators";
//=============================================================================
static const nlsGateway gateway[] = {
    { "mustBeNumericOrLogical", (void*)Nelson::ValidatorsGateway::mustBeNumericOrLogicalBuiltin, 0,
        -1, CPP_BUILTIN },
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
    { "mustBeText", (void*)Nelson::ValidatorsGateway::mustBeTextBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeFolder", (void*)Nelson::ValidatorsGateway::mustBeFolderBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeFile", (void*)Nelson::ValidatorsGateway::mustBeFileBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeVector", (void*)Nelson::ValidatorsGateway::mustBeVectorBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeFloat", (void*)Nelson::ValidatorsGateway::mustBeFloatBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNumeric", (void*)Nelson::ValidatorsGateway::mustBeNumericBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNonempty", (void*)Nelson::ValidatorsGateway::mustBeNonemptyBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeA", (void*)Nelson::ValidatorsGateway::mustBeABuiltin, 0, -2, CPP_BUILTIN },
    { "mustBePositive", (void*)Nelson::ValidatorsGateway::mustBePositiveBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonnegative", (void*)Nelson::ValidatorsGateway::mustBeNonnegativeBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNegative", (void*)Nelson::ValidatorsGateway::mustBeNegativeBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonpositive", (void*)Nelson::ValidatorsGateway::mustBeNonpositiveBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonNan", (void*)Nelson::ValidatorsGateway::mustBeNonNanBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNonZero", (void*)Nelson::ValidatorsGateway::mustBeNonZeroBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNonSparse", (void*)Nelson::ValidatorsGateway::mustBeNonSparseBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeReal", (void*)Nelson::ValidatorsGateway::mustBeRealBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeInteger", (void*)Nelson::ValidatorsGateway::mustBeIntegerBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNonmissing", (void*)Nelson::ValidatorsGateway::mustBeNonmissingBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeGreaterThan", (void*)Nelson::ValidatorsGateway::mustBeGreaterThanBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeLessThan", (void*)Nelson::ValidatorsGateway::mustBeLessThanBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeGreaterThanOrEqual", (void*)Nelson::ValidatorsGateway::mustBeGreaterThanOrEqualBuiltin,
        0, -2, CPP_BUILTIN },
    { "mustBeLessThanOrEqual", (void*)Nelson::ValidatorsGateway::mustBeLessThanOrEqualBuiltin, 0,
        -2, CPP_BUILTIN },

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
