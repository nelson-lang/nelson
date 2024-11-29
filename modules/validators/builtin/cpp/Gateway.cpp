//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
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
#include "mustBeMatrixBuiltin.hpp"
#include "mustBeRowBuiltin.hpp"
#include "mustBeColumnBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"validators";
//=============================================================================
static const nlsGateway gateway[] = {
    //=============================================================================
    { "mustBeFile", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFileBuiltin, 0, -1 },
    { "mustBeNumericOrLogical",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNumericOrLogicalBuiltin, 0, -1 },
    { "mustBeLogical", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLogicalBuiltin, 0, -1 },
    { "mustBeLogicalScalar", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLogicalScalarBuiltin, 0,
        -1 },
    { "mustBeFinite", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFiniteBuiltin, 0, -1 },
    { "mustBeScalarOrEmpty", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeScalarOrEmptyBuiltin, 0,
        -1 },
    { "mustBeValidVariableName",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeValidVariableNameBuiltin, 0, -1 },
    { "mustBeTextScalar", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeTextScalarBuiltin, 0, -1 },
    { "mustBeText", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeTextBuiltin, 0, -1 },
    { "mustBeNonzeroLengthText",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonzeroLengthTextBuiltin, 0, -1 },
    { "mustBeFolder", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFolderBuiltin, 0, -1 },
    { "mustBeVector", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeVectorBuiltin, 0, -1 },
    { "mustBeFloat", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeFloatBuiltin, 0, -1 },
    { "mustBeNumeric", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNumericBuiltin, 0, -1 },
    { "mustBeNonempty", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonemptyBuiltin, 0, -1 },
    { "mustBeA", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeABuiltin, 0, -2 },
    { "mustBePositive", (ptrBuiltin)Nelson::ValidatorsGateway::mustBePositiveBuiltin, 0, -1 },
    { "mustBeNonnegative", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonnegativeBuiltin, 0, -1 },
    { "mustBeNegative", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNegativeBuiltin, 0, -1 },
    { "mustBeNonpositive", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonpositiveBuiltin, 0, -1 },
    { "mustBeNonNan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonNanBuiltin, 0, -1 },
    { "mustBeNonZero", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonZeroBuiltin, 0, -1 },
    { "mustBeNonSparse", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonSparseBuiltin, 0, -1 },
    { "mustBeReal", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeRealBuiltin, 0, -1 },
    { "mustBeInteger", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeIntegerBuiltin, 0, -1 },
    { "mustBeNonmissing", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeNonmissingBuiltin, 0, -1 },
    { "mustBeGreaterThan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeGreaterThanBuiltin, 0, -2 },
    { "mustBeLessThan", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLessThanBuiltin, 0, -2 },
    { "mustBeGreaterThanOrEqual",
        (ptrBuiltin)Nelson::ValidatorsGateway::mustBeGreaterThanOrEqualBuiltin, 0, -2 },
    { "mustBeLessThanOrEqual", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeLessThanOrEqualBuiltin,
        0, -2 },
    { "mustBeMember", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeMemberBuiltin, 0, -2 },
    { "mustBeInRange", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeInRangeBuiltin, 0, -3 },
    { "mustBeMatrix", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeMatrixBuiltin, 0, -1 },
    { "mustBeRow", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeRowBuiltin, 0, -1 },
    { "mustBeColumn", (ptrBuiltin)Nelson::ValidatorsGateway::mustBeColumnBuiltin, 0, -1 },

};
//=============================================================================
static bool
initializeValidatorsModule(Nelson::Evaluator* eval)
{
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
