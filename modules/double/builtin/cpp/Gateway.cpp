//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "double_plus_doubleBuiltin.hpp"
#include "double_minus_doubleBuiltin.hpp"
#include "double_dispBuiltin.hpp"
#include "ndarraydouble_dispBuiltin.hpp"
#include "double_uminusBuiltin.hpp"
#include "doubleBuiltin.hpp"
#include "double_colon_doubleBuiltin.hpp"
#include "colon_double_double_doubleBuiltin.hpp"
#include "double_mtimes_doubleBuiltin.hpp"
#include "double_times_doubleBuiltin.hpp"
#include "double_gt_doubleBuiltin.hpp"
#include "double_ne_doubleBuiltin.hpp"
#include "double_lt_doubleBuiltin.hpp"
#include "double_le_doubleBuiltin.hpp"
#include "double_horzcat_doubleBuiltin.hpp"
#include "double_vertcat_doubleBuiltin.hpp"
#include "ndarraydouble_horzcat_ndarraydoubleBuiltin.hpp"
#include "ndarraydouble_vertcat_ndarraydoubleBuiltin.hpp"
#include "double_isequalBuiltin.hpp"
#include "double_isequalnBuiltin.hpp"
#include "ndarraydouble_isequalBuiltin.hpp"
#include "ndarraydouble_isequalnBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"double";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "double_plus_double", Nelson::DoubleGateway::double_plus_doubleBuiltin, 1, 2 },
    { "double_minus_double", Nelson::DoubleGateway::double_minus_doubleBuiltin, 1, 2 },
    { "double_disp", Nelson::DoubleGateway::double_dispBuiltin, 0, 1 },
    { "ndarraydouble_disp", Nelson::DoubleGateway::ndarraydouble_dispBuiltin, 0, 1 },
    { "double_uminus", Nelson::DoubleGateway::double_uminusBuiltin, 1, 1 },
    { "double", Nelson::DoubleGateway::doubleBuiltin, 1, 1 },
    { "double_colon_double", Nelson::DoubleGateway::double_colon_doubleBuiltin, 1, 2 },
    { "colon_double_double_double", Nelson::DoubleGateway::colon_double_double_doubleBuiltin, 1, 3 },
    { "double_mtimes_double", Nelson::DoubleGateway::double_mtimes_doubleBuiltin, 1, 2 },
    { "double_times_double", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "ndarraydouble_times_ndarraydouble", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "double_times_ndarraydouble", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "ndarraydouble_times_double", Nelson::DoubleGateway::double_times_doubleBuiltin, 1, 2 },
    { "double_gt_double", Nelson::DoubleGateway::double_gt_doubleBuiltin, 1, 2 },
    { "double_lt_double", Nelson::DoubleGateway::double_lt_doubleBuiltin, 1, 2 },
    { "double_le_double", Nelson::DoubleGateway::double_le_doubleBuiltin, 1, 2 },
    { "double_ne_double", Nelson::DoubleGateway::double_ne_doubleBuiltin, 1, 2 },
    { "double_vertcat_double", Nelson::DoubleGateway::double_vertcat_doubleBuiltin, 1, 2 },
    { "double_horzcat_double", Nelson::DoubleGateway::double_horzcat_doubleBuiltin, 1, 2 },
    { "ndarraydouble_vertcat_ndarraydouble", Nelson::DoubleGateway::ndarraydouble_vertcat_ndarraydoubleBuiltin, 1, 2 },
    { "ndarraydouble_horzcat_ndarraydouble", Nelson::DoubleGateway::ndarraydouble_horzcat_ndarraydoubleBuiltin, 1, 2 },
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
