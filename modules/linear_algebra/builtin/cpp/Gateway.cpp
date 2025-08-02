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
#include "expmBuiltin.hpp"
#include "invBuiltin.hpp"
#include "issymmetricBuiltin.hpp"
#include "ishermitianBuiltin.hpp"
#include "logmBuiltin.hpp"
#include "rcondBuiltin.hpp"
#include "schurBuiltin.hpp"
#include "sqrtmBuiltin.hpp"
#include "svdBuiltin.hpp"
#include "traceBuiltin.hpp"
#include "detBuiltin.hpp"
#include "eigBuiltin.hpp"
#include "cholBuiltin.hpp"
#include "balanceBuiltin.hpp"
#include "luBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"linear_algebra";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sqrtm", (ptrBuiltin)Nelson::LinearAlgebraGateway::sqrtmBuiltin, 1, 1 },
    { "logm", (ptrBuiltin)Nelson::LinearAlgebraGateway::logmBuiltin, 1, 1 },
    { "expm", (ptrBuiltin)Nelson::LinearAlgebraGateway::expmBuiltin, 1, 1 },
    { "schur", (ptrBuiltin)Nelson::LinearAlgebraGateway::schurBuiltin, 2, 2 },
    { "inv", (ptrBuiltin)Nelson::LinearAlgebraGateway::invBuiltin, 1, 1 },
    { "trace", (ptrBuiltin)Nelson::LinearAlgebraGateway::traceBuiltin, 1, 1 },
    { "svd", (ptrBuiltin)Nelson::LinearAlgebraGateway::svdBuiltin, 3, 2 },
    { "rcond", (ptrBuiltin)Nelson::LinearAlgebraGateway::rcondBuiltin, 1, 1 },
    { "issymmetric", (ptrBuiltin)Nelson::LinearAlgebraGateway::issymmetricBuiltin, 1, 2 },
    { "ishermitian", (ptrBuiltin)Nelson::LinearAlgebraGateway::ishermitianBuiltin, 1, 2 },
    { "det", (ptrBuiltin)Nelson::LinearAlgebraGateway::detBuiltin, 1, 1 },
    { "eig", (ptrBuiltin)Nelson::LinearAlgebraGateway::eigBuiltin, 2, 2 },
    { "chol", (ptrBuiltin)Nelson::LinearAlgebraGateway::cholBuiltin, 1, 1 },
    { "balance", (ptrBuiltin)Nelson::LinearAlgebraGateway::balanceBuiltin, 3, 2 },
    { "lu", (ptrBuiltin)Nelson::LinearAlgebraGateway::luBuiltin, 3, 1 },
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
