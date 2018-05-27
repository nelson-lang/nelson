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
#include "double_dispBuiltin.hpp"
#include "ndarraydouble_dispBuiltin.hpp"
#include "doubleBuiltin.hpp"
#include "double_colon_doubleBuiltin.hpp"
#include "colon_double_double_doubleBuiltin.hpp"
#include "double_mtimes_doubleBuiltin.hpp"
#include "double_times_doubleBuiltin.hpp"
#include "double_isequalBuiltin.hpp"
#include "double_isequalnBuiltin.hpp"
#include "ndarraydouble_isequalBuiltin.hpp"
#include "ndarraydouble_isequalnBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"double";
//=============================================================================
static const nlsGateway gateway[] = {
    { "double_disp", Nelson::DoubleGateway::double_dispBuiltin, 0, 1 },
    { "ndarraydouble_disp", Nelson::DoubleGateway::ndarraydouble_dispBuiltin, 0, 1 },
    { "double", Nelson::DoubleGateway::doubleBuiltin, 1, 1 },
    { "double_colon_double", Nelson::DoubleGateway::double_colon_doubleBuiltin, 1, 2 },
    { "colon_double_double_double", Nelson::DoubleGateway::colon_double_double_doubleBuiltin, 1,
        3 },
    { "double_mtimes_double", Nelson::DoubleGateway::double_mtimes_doubleBuiltin, 1, 2 },
    { "double_times_double", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "ndarraydouble_times_ndarraydouble", Nelson::DoubleGateway::double_times_doubleBuiltin, 1,
        2 },
    { "double_times_ndarraydouble", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "ndarraydouble_times_double", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "double_isequal", Nelson::DoubleGateway::double_isequalBuiltin, 1, 2 },
    { "double_isequaln", Nelson::DoubleGateway::double_isequalnBuiltin, 1, 2 },
    { "ndarraydouble_isequal", Nelson::DoubleGateway::ndarraydouble_isequalBuiltin, 1, 2 },
    { "ndarraydouble_isequaln", Nelson::DoubleGateway::ndarraydouble_isequalnBuiltin, 1, 2 },
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
