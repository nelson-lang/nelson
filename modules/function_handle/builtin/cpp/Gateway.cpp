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
    { "func2str", (void*)Nelson::FunctionHandleGateway::func2strBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "function_handle_extraction",
        (void*)Nelson::FunctionHandleGateway::function_handle_extractionBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "function_handle_disp", (void*)Nelson::FunctionHandleGateway::function_handle_dispBuiltin, 0,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isfunction_handle", (void*)Nelson::FunctionHandleGateway::isfunction_handleBuiltin, 1, 1 },
    { "function_handle_fieldnames",
        (void*)Nelson::FunctionHandleGateway::function_handle_fieldnamesBuiltin, 1, 1 },
    { "function_handle_isequal",
        (void*)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "function_handle_isequaln",
        (void*)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "function_handle_isequalto",
        (void*)Nelson::FunctionHandleGateway::function_handle_isequalBuiltin, 1, 2 },
    { "str2func", (void*)Nelson::FunctionHandleGateway::str2funcBuiltin, 1, 1 },
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
