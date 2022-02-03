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
#include "bannerBuiltin.hpp"
#include "inserthtmlBuiltin.hpp"
#include "qt_verboseBuiltin.hpp"
#include "uigetdirBuiltin.hpp"
#include "lookandfeelBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"gui";
//=============================================================================
static const nlsGateway gateway[] = {
    { "banner", (ptrBuiltin)Nelson::GuiGateway::bannerBuiltin, 0, 0, CPP_BUILTIN_WITH_EVALUATOR },
    { "inserthtml", (ptrBuiltin)Nelson::GuiGateway::inserthtmlBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lookandfeel", (ptrBuiltin)Nelson::GuiGateway::lookandfeelBuiltin, 1, 2 },
    { "uigetdir", (ptrBuiltin)Nelson::GuiGateway::uigetdirBuiltin, 1, 2 },
    { "qt_verbose", (ptrBuiltin)Nelson::GuiGateway::qt_verboseBuiltin, 1, 1 },
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
