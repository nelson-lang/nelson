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
#include "__stlread__Builtin.hpp"
#include "__stlwrite__Builtin.hpp"
#include "convhullnBuiltin.hpp"
#include "delaunaynBuiltin.hpp"
#include "voronoinBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"geometry";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__stlread__", (ptrBuiltin)Nelson::GeometryGateway::__stlread__Builtin, 5, 1 },
    { "__stlwrite__", (ptrBuiltin)Nelson::GeometryGateway::__stlwrite__Builtin, 0, 6 },
    { "convhulln", (ptrBuiltin)Nelson::GeometryGateway::convhullnBuiltin, 2, 2 },
    { "delaunayn", (ptrBuiltin)Nelson::GeometryGateway::delaunaynBuiltin, 1, 2 },
    { "voronoin", (ptrBuiltin)Nelson::GeometryGateway::voronoinBuiltin, 2, 2 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
