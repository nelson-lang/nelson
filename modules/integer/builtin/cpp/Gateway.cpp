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
#include "int8Builtin.hpp"
#include "int16Builtin.hpp"
#include "int32Builtin.hpp"
#include "int64Builtin.hpp"
#include "uint8Builtin.hpp"
#include "uint16Builtin.hpp"
#include "uint32Builtin.hpp"
#include "uint64Builtin.hpp"
#include "intmaxBuiltin.hpp"
#include "intminBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"integer";
//=============================================================================
static const nlsGateway gateway[] = {
    { "int8", (ptrBuiltin)Nelson::IntegerGateway::int8Builtin, 1, 1 },
    { "int16", (ptrBuiltin)Nelson::IntegerGateway::int16Builtin, 1, 1 },
    { "int32", (ptrBuiltin)Nelson::IntegerGateway::int32Builtin, 1, 1 },
    { "int64", (ptrBuiltin)Nelson::IntegerGateway::int64Builtin, 1, 1 },
    { "uint8", (ptrBuiltin)Nelson::IntegerGateway::uint8Builtin, 1, 1 },
    { "uint16", (ptrBuiltin)Nelson::IntegerGateway::uint16Builtin, 1, 1 },
    { "uint32", (ptrBuiltin)Nelson::IntegerGateway::uint32Builtin, 1, 1 },
    { "uint64", (ptrBuiltin)Nelson::IntegerGateway::uint64Builtin, 1, 1 },
    { "intmax", (ptrBuiltin)Nelson::IntegerGateway::intmaxBuiltin, -1, -1 },
    { "intmin", (ptrBuiltin)Nelson::IntegerGateway::intminBuiltin, -1, -1 },
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
