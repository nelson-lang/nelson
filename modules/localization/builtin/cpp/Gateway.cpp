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
#include "getavailablelanguagesBuiltin.hpp"
#include "getdefaultlanguageBuiltin.hpp"
#include "getlanguageBuiltin.hpp"
#include "setlanguageBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"localization";
//=============================================================================
static const nlsGateway gateway[] = {
    { "getavailablelanguages", (void*)Nelson::LocalizationGateway::getavailablelanguagesBuiltin, 1, 0,
        CPP_BUILTIN },
    { "setlanguage", (void*)Nelson::LocalizationGateway::setlanguageBuiltin, 1, 1, CPP_BUILTIN },
    { "getlanguage", (void*)Nelson::LocalizationGateway::getlanguageBuiltin, 1, 0, CPP_BUILTIN },
    { "getdefaultlanguage", (void*)Nelson::LocalizationGateway::getdefaultlanguageBuiltin, 1, 0,
        CPP_BUILTIN },
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
