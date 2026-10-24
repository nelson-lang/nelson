//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "doubleBuiltin.hpp"
#include "flintmaxBuiltin.hpp"
#include "realmaxBuiltin.hpp"
#include "realminBuiltin.hpp"
#include "ToDouble.hpp"
#include "ArrayOfAssignConversion.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"double";
//=============================================================================
static const nlsGateway gateway[] = {
    { "double", (ptrBuiltin)Nelson::DoubleGateway::doubleBuiltin, 1, 1 },
    { "flintmax", (ptrBuiltin)Nelson::DoubleGateway::flintmaxBuiltin, 1, 1 },
    { "realmax", (ptrBuiltin)Nelson::DoubleGateway::realmaxBuiltin, 1, 1 },
    { "realmin", (ptrBuiltin)Nelson::DoubleGateway::realminBuiltin, 1, 1 },
};
//=============================================================================
static ArrayOf
stringToDoubleForAssignment(const ArrayOf& value)
{
    bool needToOverload = false;
    return ToDouble(value, needToOverload);
}
//=============================================================================
// Indexed assignment of text into a double array (types module) parses it
// with double(): registered at startup so that it never depends on whether
// double() was already called.
static bool
initializeDoubleModule(Nelson::Evaluator* eval)
{
    setStringToDoubleAssignmentConverter(stringToDoubleForAssignment);
    return true;
}
//=============================================================================
static bool
finishDoubleModule(Nelson::Evaluator* eval)
{
    setStringToDoubleAssignmentConverter(nullptr);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeDoubleModule)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishDoubleModule)
//=============================================================================
