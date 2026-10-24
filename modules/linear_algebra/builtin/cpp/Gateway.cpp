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
#include "pagemtimesBuiltin.hpp"
#include "pagetransposeBuiltin.hpp"
#include "pageinvBuiltin.hpp"
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
#include "eigsBuiltin.hpp"
#include "cholBuiltin.hpp"
#include "balanceBuiltin.hpp"
#include "luBuiltin.hpp"
#include "qrBuiltin.hpp"
#include "svdsBuiltin.hpp"
#include "pcgBuiltin.hpp"
#include "bicgBuiltin.hpp"
#include "bicgstabBuiltin.hpp"
#include "cgsBuiltin.hpp"
#include "qmrBuiltin.hpp"
#include "gmresBuiltin.hpp"
#include "lsqrBuiltin.hpp"
#include "lsmrBuiltin.hpp"
#include "minresBuiltin.hpp"
#include "icholBuiltin.hpp"
#include "iluBuiltin.hpp"
#include "trapzBuiltin.hpp"
#include "cumtrapzBuiltin.hpp"
#include "diffBuiltin.hpp"
#include "kronBuiltin.hpp"
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
    { "pagemtimes", (ptrBuiltin)Nelson::LinearAlgebraGateway::pagemtimesBuiltin, 1, 4 },
    { "pagetranspose", (ptrBuiltin)Nelson::LinearAlgebraGateway::pagetransposeBuiltin, 1, 1 },
    { "pageinv", (ptrBuiltin)Nelson::LinearAlgebraGateway::pageinvBuiltin, 1, 1 },
    { "trace", (ptrBuiltin)Nelson::LinearAlgebraGateway::traceBuiltin, 1, 1 },
    { "svd", (ptrBuiltin)Nelson::LinearAlgebraGateway::svdBuiltin, 3, 2 },
    { "rcond", (ptrBuiltin)Nelson::LinearAlgebraGateway::rcondBuiltin, 1, 1 },
    { "issymmetric", (ptrBuiltin)Nelson::LinearAlgebraGateway::issymmetricBuiltin, 1, 2 },
    { "ishermitian", (ptrBuiltin)Nelson::LinearAlgebraGateway::ishermitianBuiltin, 1, 2 },
    { "det", (ptrBuiltin)Nelson::LinearAlgebraGateway::detBuiltin, 1, 1 },
    { "eig", (ptrBuiltin)Nelson::LinearAlgebraGateway::eigBuiltin, 2, 2 },
    { "eigs", (ptrBuiltin)Nelson::LinearAlgebraGateway::eigsBuiltin, 2, 2 },
    { "chol", (ptrBuiltin)Nelson::LinearAlgebraGateway::cholBuiltin, 2, 1 },
    { "balance", (ptrBuiltin)Nelson::LinearAlgebraGateway::balanceBuiltin, 3, 2 },
    { "lu", (ptrBuiltin)Nelson::LinearAlgebraGateway::luBuiltin, 3, 1 },
    { "qr", (ptrBuiltin)Nelson::LinearAlgebraGateway::qrBuiltin, 3, 3 },
    { "svds", (ptrBuiltin)Nelson::LinearAlgebraGateway::svdsBuiltin, 3, 2 },
    { "pcg", (ptrBuiltin)Nelson::LinearAlgebraGateway::pcgBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bicg", (ptrBuiltin)Nelson::LinearAlgebraGateway::bicgBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bicgstab", (ptrBuiltin)Nelson::LinearAlgebraGateway::bicgstabBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cgs", (ptrBuiltin)Nelson::LinearAlgebraGateway::cgsBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "qmr", (ptrBuiltin)Nelson::LinearAlgebraGateway::qmrBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "gmres", (ptrBuiltin)Nelson::LinearAlgebraGateway::gmresBuiltin, 5, 8,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lsqr", (ptrBuiltin)Nelson::LinearAlgebraGateway::lsqrBuiltin, 6, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lsmr", (ptrBuiltin)Nelson::LinearAlgebraGateway::lsmrBuiltin, 6, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "minres", (ptrBuiltin)Nelson::LinearAlgebraGateway::minresBuiltin, 5, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ichol", (ptrBuiltin)Nelson::LinearAlgebraGateway::icholBuiltin, 1, 2 },
    { "ilu", (ptrBuiltin)Nelson::LinearAlgebraGateway::iluBuiltin, 3, 2 },
    { "trapz", (ptrBuiltin)Nelson::LinearAlgebraGateway::trapzBuiltin, 1, 3 },
    { "cumtrapz", (ptrBuiltin)Nelson::LinearAlgebraGateway::cumtrapzBuiltin, 1, 3 },
    { "diff", (ptrBuiltin)Nelson::LinearAlgebraGateway::diffBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "kron", (ptrBuiltin)Nelson::LinearAlgebraGateway::kronBuiltin, 1, 2, CPP_BUILTIN },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
