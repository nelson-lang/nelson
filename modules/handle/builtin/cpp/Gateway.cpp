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
    { "delete", Nelson::HandleGateway::deleteBuiltin, 0, 1 },
    { "get", Nelson::HandleGateway::getBuiltin, 1, 1 },
    { "set", Nelson::HandleGateway::setBuiltin, 1, 1 },
    { "invoke", Nelson::HandleGateway::invokeBuiltin, 1, 2 },
    { "isvalid", Nelson::HandleGateway::isvalidBuiltin, 1, 1 },
    { "methods", Nelson::HandleGateway::methodsBuiltin, 1, 1 },
    { "properties", Nelson::HandleGateway::propertiesBuiltin, 1, 1 },
    { "isprop", Nelson::HandleGateway::ispropBuiltin, 1, 2 },
    { "ismethod", Nelson::HandleGateway::ismethodBuiltin, 1, 2 },

    { "handle_vertcat_handle", Nelson::HandleGateway::handle_vertcat_handleBuiltin, 1, 2 },
    { "handle_horzcat_handle", Nelson::HandleGateway::handle_horzcat_handleBuiltin, 1, 2 },
    { "handle_eq_handle", Nelson::HandleGateway::handle_eq_handleBuiltin, 1, 2 },
    { "generic_eq_handle", Nelson::HandleGateway::generic_eq_handleBuiltin, 1, 2 },
    { "handle_eq_generic", Nelson::HandleGateway::handle_eq_genericBuiltin, 1, 2 },
    { "handle_isequal", Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },
    { "handle_isequaln", Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },
    { "handle_isequalto", Nelson::HandleGateway::handle_isequalBuiltin, 1, 2 },

    { "handle_get", Nelson::HandleGateway::handle_getBuiltin, 1, 1 },
    { "handle_set", Nelson::HandleGateway::handle_setBuiltin, 1, 1 },
    { "handle_invoke", Nelson::HandleGateway::handle_invokeBuiltin, 1, 1 },
    { "handle_disp", Nelson::HandleGateway::handle_dispBuiltin, 0, 1 },
    { "handle_delete", Nelson::HandleGateway::handle_deleteBuiltin, 0, 1 },
    { "handle_isvalid", Nelson::HandleGateway::handle_isvalidBuiltin, 1, 1 },
    { "handle_fieldnames", Nelson::HandleGateway::handle_fieldnamesBuiltin, 1, 1 },
    { "handle_methods", Nelson::HandleGateway::handle_methodsBuiltin, 1, 1 },
    { "handle_ismethod", Nelson::HandleGateway::handle_ismethodBuiltin, 1, 2 },
    { "handle_isprop", Nelson::HandleGateway::handle_ispropBuiltin, 1, 2 },
    { "handle_properties", Nelson::HandleGateway::handle_propertiesBuiltin, 1, 1 },

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
