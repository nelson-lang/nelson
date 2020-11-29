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
#include "logmBuiltin.hpp"
#include "rcondBuiltin.hpp"
#include "schurBuiltin.hpp"
#include "sqrtmBuiltin.hpp"
#include "svdBuiltin.hpp"
#include "traceBuiltin.hpp"
#include "detBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"linear_algebra";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sqrtm", (void*)Nelson::LinearAlgebraGateway::sqrtmBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "logm", (void*)Nelson::LinearAlgebraGateway::logmBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "expm", (void*)Nelson::LinearAlgebraGateway::expmBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "schur", (void*)Nelson::LinearAlgebraGateway::schurBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "inv", (void*)Nelson::LinearAlgebraGateway::invBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "trace", (void*)Nelson::LinearAlgebraGateway::traceBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "svd", (void*)Nelson::LinearAlgebraGateway::svdBuiltin, 3, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "rcond", (void*)Nelson::LinearAlgebraGateway::rcondBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "issymmetric", (void*)Nelson::LinearAlgebraGateway::issymmetricBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "det", (void*)Nelson::LinearAlgebraGateway::detBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
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
