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
#include "prodBuiltin.hpp"
#include "sumBuiltin.hpp"
#include "ismissingBuiltin.hpp"
#include "sortBuiltin.hpp"
#include "maxBuiltin.hpp"
#include "minBuiltin.hpp"
#include "conv2Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"data_analysis";
//=============================================================================
static const nlsGateway gateway[] = {
    { "prod", (ptrBuiltin)Nelson::DataAnalysisGateway::prodBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sum", (ptrBuiltin)Nelson::DataAnalysisGateway::sumBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ismissing", (ptrBuiltin)Nelson::DataAnalysisGateway::ismissingBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sort", (ptrBuiltin)Nelson::DataAnalysisGateway::sortBuiltin, 2, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "max", (ptrBuiltin)Nelson::DataAnalysisGateway::maxBuiltin, 2, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "min", (ptrBuiltin)Nelson::DataAnalysisGateway::minBuiltin, 2, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "conv2", (ptrBuiltin)Nelson::DataAnalysisGateway::conv2Builtin, 1, 4,
        CPP_BUILTIN_WITH_EVALUATOR },

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
