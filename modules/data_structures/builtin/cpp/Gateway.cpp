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
#include "structBuiltin.hpp"
#include "iscellstrBuiltin.hpp"
#include "cellBuiltin.hpp"
#include "fieldnamesBuiltin.hpp"
#include "struct2cellBuiltin.hpp"
#include "cell2structBuiltin.hpp"
#include "cellfunBuiltin.hpp"
#include "isfieldBuiltin.hpp"
#include "namedargs2cellBuiltin.hpp"
#include "getfieldBuiltin.hpp"
#include "rmfieldBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"data_structures";
//=============================================================================
static const nlsGateway gateway[] = {
    { "isfield", (void*)Nelson::DataStructuresGateway::isfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fieldnames", (void*)Nelson::DataStructuresGateway::fieldnamesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cellfun", (void*)Nelson::DataStructuresGateway::cellfunBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "namedargs2cell", (void*)Nelson::DataStructuresGateway::namedargs2cellBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "getfield", (void*)Nelson::DataStructuresGateway::getfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rmfield", (void*)Nelson::DataStructuresGateway::rmfieldBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "struct", (void*)Nelson::DataStructuresGateway::structBuiltin, 1, 1 },
    { "iscellstr", (void*)Nelson::DataStructuresGateway::iscellstrBuiltin, 1, 1 },
    { "cell", (void*)Nelson::DataStructuresGateway::cellBuiltin, 1, 0 },
    { "struct2cell", (void*)Nelson::DataStructuresGateway::struct2cellBuiltin, 1, 1 },
    { "cell2struct", (void*)Nelson::DataStructuresGateway::cell2structBuiltin, 1, 3 },
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
