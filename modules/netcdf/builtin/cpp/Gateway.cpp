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
#include "netcdfAbortBuiltin.hpp"
#include "netcdfBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"netcdf";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__netcdf__", (ptrBuiltin)Nelson::NetcdfGateway::netcdfBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "netcdf.abort", (ptrBuiltin)Nelson::NetcdfGateway::netcdfAbortBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
