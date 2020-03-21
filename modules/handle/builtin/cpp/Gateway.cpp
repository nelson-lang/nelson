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
//=============================================================================
#include "deleteBuiltin.hpp"
#include "getBuiltin.hpp"
#include "invokeBuiltin.hpp"
#include "ismethodBuiltin.hpp"
#include "ispropBuiltin.hpp"
#include "isvalidBuiltin.hpp"
#include "methodsBuiltin.hpp"
#include "propertiesBuiltin.hpp"
#include "setBuiltin.hpp"
//=============================================================================
#include "generic_eq_handleBuiltin.hpp"
#include "handle_deleteBuiltin.hpp"
#include "handle_dispBuiltin.hpp"
#include "handle_eq_genericBuiltin.hpp"
#include "handle_eq_handleBuiltin.hpp"
#include "handle_fieldnamesBuiltin.hpp"
#include "handle_getBuiltin.hpp"
#include "handle_horzcat_handleBuiltin.hpp"
#include "handle_invokeBuiltin.hpp"
#include "handle_isequalBuiltin.hpp"
#include "handle_ismethodBuiltin.hpp"
#include "handle_ispropBuiltin.hpp"
#include "handle_isvalidBuiltin.hpp"
#include "handle_methodsBuiltin.hpp"
#include "handle_propertiesBuiltin.hpp"
#include "handle_setBuiltin.hpp"
#include "handle_vertcat_handleBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"handle";
//=============================================================================
static const nlsGateway gateway[] = {
    { "delete", (void*)Nelson::HandleGateway::deleteBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "get", (void*)Nelson::HandleGateway::getBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "set", (void*)Nelson::HandleGateway::setBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "invoke", (void*)Nelson::HandleGateway::invokeBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "isvalid", (void*)Nelson::HandleGateway::isvalidBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "methods", (void*)Nelson::HandleGateway::methodsBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "properties", (void*)Nelson::HandleGateway::propertiesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isprop", (void*)Nelson::HandleGateway::ispropBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ismethod", (void*)Nelson::HandleGateway::ismethodBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },

    { "handle_vertcat_handle", (void*)Nelson::HandleGateway::handle_vertcat_handleBuiltin, 1, 2 },
    { "handle_horzcat_handle", (void*)Nelson::HandleGateway::handle_horzcat_handleBuiltin, 1, 2 },
    { "handle_eq_handle", (void*)Nelson::HandleGateway::handle_eq_handleBuiltin, 1, 2 },
    { "generic_eq_handle", (void*)Nelson::HandleGateway::generic_eq_handleBuiltin, 1, 2 },
    { "handle_eq_generic", (void*)Nelson::HandleGateway::handle_eq_genericBuiltin, 1, 2 },
    { "handle_isequal", (void*)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },
    { "handle_isequaln", (void*)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },
    { "handle_isequalto", (void*)Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },

    { "handle_get", (void*)Nelson::HandleGateway::handle_getBuiltin, 1, 1 },
    { "handle_set", (void*)Nelson::HandleGateway::handle_setBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_invoke", (void*)Nelson::HandleGateway::handle_invokeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_disp", (void*)Nelson::HandleGateway::handle_dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_delete", (void*)Nelson::HandleGateway::handle_deleteBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_isvalid", (void*)Nelson::HandleGateway::handle_isvalidBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_fieldnames", (void*)Nelson::HandleGateway::handle_fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "handle_methods", (void*)Nelson::HandleGateway::handle_methodsBuiltin, 1, 1 },
    { "handle_ismethod", (void*)Nelson::HandleGateway::handle_ismethodBuiltin, 1, 2 },
    { "handle_isprop", (void*)Nelson::HandleGateway::handle_ispropBuiltin, 1, 2 },
    { "handle_properties", (void*)Nelson::HandleGateway::handle_propertiesBuiltin, 1, 1 },
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
