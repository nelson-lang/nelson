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
#include "loadmatBuiltin.hpp"
#include "savematBuiltin.hpp"
#include "ismatfileBuiltin.hpp"
#include "whosmatBuiltin.hpp"
#include "whomatBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"matio";
//=============================================================================
static const nlsGateway gateway[] = {
    { "savemat", (void*)Nelson::MatioGateway::savematBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "loadmat", (void*)Nelson::MatioGateway::loadmatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "whosmat", (void*)Nelson::MatioGateway::whosmatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "whomat", (void*)Nelson::MatioGateway::whomatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "ismatfile", (void*)Nelson::MatioGateway::ismatfileBuiltin, 1, 1, CPP_BUILTIN },
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
