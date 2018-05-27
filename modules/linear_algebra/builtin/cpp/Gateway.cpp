//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"linear_algebra";
//=============================================================================
static const nlsGateway gateway[] = {
    { "sqrtm", Nelson::LinearAlgebraGateway::sqrtmBuiltin, 1, 1 },
    { "logm", Nelson::LinearAlgebraGateway::logmBuiltin, 1, 1 },
    { "expm", Nelson::LinearAlgebraGateway::expmBuiltin, 1, 1 },
    { "schur", Nelson::LinearAlgebraGateway::schurBuiltin, 2, 2 },
    { "inv", Nelson::LinearAlgebraGateway::invBuiltin, 1, 1 },
    { "trace", Nelson::LinearAlgebraGateway::traceBuiltin, 1, 1 },
    { "svd", Nelson::LinearAlgebraGateway::svdBuiltin, 3, 2 },
    { "rcond", Nelson::LinearAlgebraGateway::rcondBuiltin, 1, 1 },
    { "issymmetric", Nelson::LinearAlgebraGateway::issymmetricBuiltin, 1, 2 },
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
