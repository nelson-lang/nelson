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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"linear_algebra";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sqrtm", (ptrBuiltin)Nelson::LinearAlgebraGateway::sqrtmBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "logm", (ptrBuiltin)Nelson::LinearAlgebraGateway::logmBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "expm", (ptrBuiltin)Nelson::LinearAlgebraGateway::expmBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "schur", (ptrBuiltin)Nelson::LinearAlgebraGateway::schurBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "inv", (ptrBuiltin)Nelson::LinearAlgebraGateway::invBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "trace", (ptrBuiltin)Nelson::LinearAlgebraGateway::traceBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "svd", (ptrBuiltin)Nelson::LinearAlgebraGateway::svdBuiltin, 3, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rcond", (ptrBuiltin)Nelson::LinearAlgebraGateway::rcondBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "issymmetric", (ptrBuiltin)Nelson::LinearAlgebraGateway::issymmetricBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ishermitian", (ptrBuiltin)Nelson::LinearAlgebraGateway::ishermitianBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "det", (ptrBuiltin)Nelson::LinearAlgebraGateway::detBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "eig", (ptrBuiltin)Nelson::LinearAlgebraGateway::eigBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "chol", (ptrBuiltin)Nelson::LinearAlgebraGateway::cholBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
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
