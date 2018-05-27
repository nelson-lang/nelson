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
#include "assert_checkerrorBuiltin.hpp"
#include "assert_isapproxBuiltin.hpp"
#include "assert_isequalBuiltin.hpp"
#include "assert_isfalseBuiltin.hpp"
#include "assert_istrueBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"assert_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "assert_istrue", Nelson::AssertFunctionsGateway::assert_istrueBuiltin, 2, 1 },
    { "assert", Nelson::AssertFunctionsGateway::assert_istrueBuiltin, 2, 1 },
    { "assert_isfalse", Nelson::AssertFunctionsGateway::assert_isfalseBuiltin, 2, 1 },
    { "assert_checkerror", Nelson::AssertFunctionsGateway::assert_checkerrorBuiltin, 2, 2 },
    { "assert_isequal", Nelson::AssertFunctionsGateway::assert_isequalBuiltin, 2, 2 },
    { "assert_isapprox", Nelson::AssertFunctionsGateway::assert_isapproxBuiltin, 2, -2 },
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
