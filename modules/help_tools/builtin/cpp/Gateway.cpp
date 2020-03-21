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
#include "headcommentsBuiltin.hpp"
#include "htmltopdfBuiltin.hpp"
#include "markdownBuiltin.hpp"
#include "xmldocbuildBuiltin.hpp"
#include "xmldoccheckerBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"help_tools";
//=============================================================================
static const nlsGateway gateway[] = {
    { "headcomments", (void*)Nelson::HelpToolsGateway::headcommentsBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "xmldocchecker", (void*)Nelson::HelpToolsGateway::xmldoccheckerBuiltin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "htmltopdf", (void*)Nelson::HelpToolsGateway::htmltopdfBuiltin, 0, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "markdown", (void*)Nelson::HelpToolsGateway::markdownBuiltin, 1, 2 },
    { "xmldocbuild", (void*)Nelson::HelpToolsGateway::xmldocbuildBuiltin, 1, 5 },

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
