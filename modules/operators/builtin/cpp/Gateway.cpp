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
#include "andBuiltin.hpp"
#include "colonBuiltin.hpp"
#include "ctransposeBuiltin.hpp"
#include "eqBuiltin.hpp"
#include "geBuiltin.hpp"
#include "gtBuiltin.hpp"
#include "horzcatBuiltin.hpp"
#include "ldivideBuiltin.hpp"
#include "leBuiltin.hpp"
#include "ltBuiltin.hpp"
#include "minusBuiltin.hpp"
#include "mldivideBuiltin.hpp"
#include "mpowerBuiltin.hpp"
#include "mrdivideBuiltin.hpp"
#include "mtimesBuiltin.hpp"
#include "neBuiltin.hpp"
#include "notBuiltin.hpp"
#include "allBuiltin.hpp"
#include "anyBuiltin.hpp"
#include "orBuiltin.hpp"
#include "plusBuiltin.hpp"
#include "powerBuiltin.hpp"
#include "rdivideBuiltin.hpp"
#include "shortcutandBuiltin.hpp"
#include "shortcutorBuiltin.hpp"
#include "subsindexBuiltin.hpp"
#include "timesBuiltin.hpp"
#include "transposeBuiltin.hpp"
#include "uminusBuiltin.hpp"
#include "uplusBuiltin.hpp"
#include "vertcatBuiltin.hpp"
#include "ismemberBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"operators";
//=============================================================================
static const nlsGateway gateway[] = {
    { "plus", (void*)Nelson::OperatorsGateway::plusBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "minus", (void*)Nelson::OperatorsGateway::minusBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uminus", (void*)Nelson::OperatorsGateway::uminusBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "uplus", (void*)Nelson::OperatorsGateway::uplusBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "and", (void*)Nelson::OperatorsGateway::andBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "colon", (void*)Nelson::OperatorsGateway::colonBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ctranspose", (void*)Nelson::OperatorsGateway::ctransposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "transpose", (void*)Nelson::OperatorsGateway::transposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "eq", (void*)Nelson::OperatorsGateway::eqBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ge", (void*)Nelson::OperatorsGateway::geBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "gt", (void*)Nelson::OperatorsGateway::gtBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "le", (void*)Nelson::OperatorsGateway::leBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "lt", (void*)Nelson::OperatorsGateway::ltBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ne", (void*)Nelson::OperatorsGateway::neBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ldivide", (void*)Nelson::OperatorsGateway::ldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rdivide", (void*)Nelson::OperatorsGateway::rdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mldivide", (void*)Nelson::OperatorsGateway::mldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mrdivide", (void*)Nelson::OperatorsGateway::mrdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mtimes", (void*)Nelson::OperatorsGateway::mtimesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "times", (void*)Nelson::OperatorsGateway::timesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "not", (void*)Nelson::OperatorsGateway::notBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "or", (void*)Nelson::OperatorsGateway::orBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "any", (void*)Nelson::OperatorsGateway::anyBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "all", (void*)Nelson::OperatorsGateway::allBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "power", (void*)Nelson::OperatorsGateway::powerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mpower", (void*)Nelson::OperatorsGateway::mpowerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "shortcutor", (void*)Nelson::OperatorsGateway::shortcutorBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "shortcutand", (void*)Nelson::OperatorsGateway::shortcutandBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "subsindex", (void*)Nelson::OperatorsGateway::subsindexBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    //"subsasgn"
    //"subsref"
    { "horzcat", (void*)Nelson::OperatorsGateway::horzcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "vertcat", (void*)Nelson::OperatorsGateway::vertcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ismember", (void*)Nelson::OperatorsGateway::ismemberBuiltin, 1, 2,
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
