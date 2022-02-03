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
#include "mustBeNonzeroLengthTextBuiltin.hpp"
#include "mustBeMemberBuiltin.hpp"
#include "mustBeInRangeBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"validators";
//=============================================================================
static const nlsGateway gateway[] = {
    { "mustBeNumericOrLogical",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNumericOrLogicalBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeLogical", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLogicalBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeLogicalScalar", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLogicalScalarBuiltin, 0,
        -1, CPP_BUILTIN },
    { "mustBeFinite", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFiniteBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeScalarOrEmpty", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeScalarOrEmptyBuiltin, 0,
        -1, CPP_BUILTIN },
    { "mustBeValidVariableName",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeValidVariableNameBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeTextScalar", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeTextScalarBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeText", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeTextBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeNonzeroLengthText",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonzeroLengthTextBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeFolder", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFolderBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeFile", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFileBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeVector", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeVectorBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeFloat", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFloatBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNumeric", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNumericBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonempty", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonemptyBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeA", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeABuiltin, 0, -2, CPP_BUILTIN },
    { "mustBePositive", (ptrBuiltin)Nelson::ValidatorsGateway::mustBePositiveBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonnegative", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonnegativeBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNegative", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNegativeBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonpositive", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonpositiveBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonNan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonNanBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonZero", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonZeroBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonSparse", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonSparseBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeReal", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeRealBuiltin, 0, -1, CPP_BUILTIN },
    { "mustBeInteger", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeIntegerBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeNonmissing", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonmissingBuiltin, 0, -1,
        CPP_BUILTIN },
    { "mustBeGreaterThan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeGreaterThanBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeLessThan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLessThanBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeGreaterThanOrEqual",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeGreaterThanOrEqualBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeLessThanOrEqual", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLessThanOrEqualBuiltin,
        0, -2, CPP_BUILTIN },
    { "mustBeMember", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeMemberBuiltin, 0, -2,
        CPP_BUILTIN },
    { "mustBeInRange", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeInRangeBuiltin, 0, -3,
        CPP_BUILTIN },
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
