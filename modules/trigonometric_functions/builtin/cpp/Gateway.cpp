//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "acosBuiltin.hpp"
#include "asinBuiltin.hpp"
#include "atanBuiltin.hpp"
#include "cosBuiltin.hpp"
#include "coshBuiltin.hpp"
#include "sinBuiltin.hpp"
#include "sinhBuiltin.hpp"
#include "tanBuiltin.hpp"
#include "tanhBuiltin.hpp"
#include "cosmBuiltin.hpp"
#include "cosmBuiltin.hpp"
#include "sinmBuiltin.hpp"
#include "tanmBuiltin.hpp"
#include "atan2Builtin.hpp"
#include "atanhBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"trigonometric_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "cos", (ptrBuiltin)Nelson::TrigonometricGateway::cosBuiltin, 1, 1 },
    { "sin", (ptrBuiltin)Nelson::TrigonometricGateway::sinBuiltin, 1, 1 },
    { "tan", (ptrBuiltin)Nelson::TrigonometricGateway::tanBuiltin, 1, 1 },
    { "cosh", (ptrBuiltin)Nelson::TrigonometricGateway::coshBuiltin, 1, 1 },
    { "sinh", (ptrBuiltin)Nelson::TrigonometricGateway::sinhBuiltin, 1, 1 },
    { "tanh", (ptrBuiltin)Nelson::TrigonometricGateway::tanhBuiltin, 1, 1 },
    { "acos", (ptrBuiltin)Nelson::TrigonometricGateway::acosBuiltin, 1, 1 },
    { "asin", (ptrBuiltin)Nelson::TrigonometricGateway::asinBuiltin, 1, 1 },
    { "atan", (ptrBuiltin)Nelson::TrigonometricGateway::atanBuiltin, 1, 1 },
    { "cosm", (ptrBuiltin)Nelson::TrigonometricGateway::cosmBuiltin, 1, 1 },
    { "sinm", (ptrBuiltin)Nelson::TrigonometricGateway::sinmBuiltin, 1, 1 },
    { "tanm", (ptrBuiltin)Nelson::TrigonometricGateway::tanmBuiltin, 1, 1 },
    { "atan2", (ptrBuiltin)Nelson::TrigonometricGateway::atan2Builtin, 1, 2 },
    { "atanh", (ptrBuiltin)Nelson::TrigonometricGateway::atanhBuiltin, 1, 1 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
