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
#include "addgatewayBuiltin.hpp"
#include "removegatewayBuiltin.hpp"
#include "gatewayinfoBuiltin.hpp"
#include "dlopenBuiltin.hpp"
#include "dlcloseBuiltin.hpp"
#include "dlsymBuiltin.hpp"
#include "dlinfoBuiltin.hpp"
#include "dllib_usedBuiltin.hpp"
#include "dllib_dispBuiltin.hpp"
#include "dllib_getBuiltin.hpp"
#include "dllib_ispropBuiltin.hpp"
#include "dllib_fieldnamesBuiltin.hpp"
#include "dllib_isvalidBuiltin.hpp"
#include "dlsym_dispBuiltin.hpp"
#include "dlsym_getBuiltin.hpp"
#include "dlsym_ispropBuiltin.hpp"
#include "dlsym_fieldnamesBuiltin.hpp"
#include "dlsym_deleteBuiltin.hpp"
#include "dlcallBuiltin.hpp"
#include "getdynlibextBuiltin.hpp"
#include "libpointerBuiltin.hpp"
#include "libpointer_dispBuiltin.hpp"
#include "libpointer_isNullBuiltin.hpp"
#include "libpointer_reshapeBuiltin.hpp"
#include "libpointer_getBuiltin.hpp"
#include "isNullBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"dynamic_link";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "addgateway", Nelson::DynamicLinkGateway::addgatewayBuiltin, 0, 1 },
    { "removegateway", Nelson::DynamicLinkGateway::removegatewayBuiltin, 0, 1 },
    { "gatewayinfo", Nelson::DynamicLinkGateway::gatewayinfoBuiltin, 2, 1 },
    { "dlopen", Nelson::DynamicLinkGateway::dlopenBuiltin, 1, 1 },
    { "dlclose", Nelson::DynamicLinkGateway::dlcloseBuiltin, 0, 1 },
    { "dlsym", Nelson::DynamicLinkGateway::dlsymBuiltin, -1, 4 },
    { "dlsym_disp", Nelson::DynamicLinkGateway::dlsym_dispBuiltin, 0, 1 },
    { "dlsym_get", Nelson::DynamicLinkGateway::dlsym_getBuiltin, 1, 2 },
    { "dlsym_isprop", Nelson::DynamicLinkGateway::dlsym_ispropBuiltin, 1, 2 },
    { "dlsym_fieldnames", Nelson::DynamicLinkGateway::dlsym_fieldnamesBuiltin, 1, 1 },
    { "dlsym_delete", Nelson::DynamicLinkGateway::dlsym_deleteBuiltin, 0, 1 },
    { "dllib_disp", Nelson::DynamicLinkGateway::dllib_dispBuiltin, 0, 1 },
    { "dllib_get", Nelson::DynamicLinkGateway::dllib_getBuiltin, 1, 2 },
    { "dllib_isprop", Nelson::DynamicLinkGateway::dllib_ispropBuiltin, 1, 2 },
    { "dllib_used", Nelson::DynamicLinkGateway::dllib_usedBuiltin, 1, 0 },
    { "dllib_delete", Nelson::DynamicLinkGateway::dlcloseBuiltin, 0, 1 },
    { "dllib_isvalid", Nelson::DynamicLinkGateway::dllib_isvalidBuiltin, 1, 1 },
    { "dllib_fieldnames", Nelson::DynamicLinkGateway::dllib_fieldnamesBuiltin, 1, 1 },
    { "dlinfo", Nelson::DynamicLinkGateway::dlinfoBuiltin, 1, 1 },
    { "dlcall", Nelson::DynamicLinkGateway::dlcallBuiltin, -1, -1 },
    { "libpointer", Nelson::DynamicLinkGateway::libpointerBuiltin, 1, -1 },
    { "libpointer_disp", Nelson::DynamicLinkGateway::libpointer_dispBuiltin, 0, 1 },
	{ "libpointer_isNull", Nelson::DynamicLinkGateway::libpointer_isNullBuiltin, 1, 1 },
	{ "libpointer_reshape", Nelson::DynamicLinkGateway::libpointer_reshapeBuiltin, 0, 3 },
	{ "libpointer_get", Nelson::DynamicLinkGateway::libpointer_getBuiltin, 1, -1 },
	{ "isNull", Nelson::DynamicLinkGateway::isNullBuiltin, 1, 1 },
	{ "getdynlibext", Nelson::DynamicLinkGateway::getdynlibextBuiltin, 1, 0 }
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
