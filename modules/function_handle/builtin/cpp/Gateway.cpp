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
#include "func2strBuiltin.hpp"
#include "function_handle_dispBuiltin.hpp"
#include "function_handle_extractionBuiltin.hpp"
#include "function_handle_fieldnamesBuiltin.hpp"
#include "function_handle_isequalBuiltin.hpp"
#include "isfunction_handleBuiltin.hpp"
#include "str2funcBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"function_handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { "str2func", Nelson::FunctionHandleGateway::str2funcBuiltin, 1, 1 },
    { "func2str", Nelson::FunctionHandleGateway::func2strBuiltin, 1, 1 },
    { "function_handle_disp", Nelson::FunctionHandleGateway::function_handle_dispBuiltin, 0, 1 },
    { "isfunction_handle", Nelson::FunctionHandleGateway::isfunction_handleBuiltin, 1, 1 },
    { "function_handle_extraction",
        Nelson::FunctionHandleGateway::function_handle_extractionBuiltin, -1, -1 },
    { "function_handle_fieldnames",
        Nelson::FunctionHandleGateway::function_handle_fieldnamesBuiltin, 1, 1 },
    { "function_handle_isequal", Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1,
        2 },
    { "function_handle_isequaln", Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1,
        2 },
    { "function_handle_isequalto", Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1,
        2 }

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
