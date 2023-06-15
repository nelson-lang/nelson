//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
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
#include "bitandBuiltin.hpp"
#include "bitorBuiltin.hpp"
#include "bitxorBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"operators";
//=============================================================================
static const nlsGateway gateway[] = {
    { "plus", (ptrBuiltin)Nelson::OperatorsGateway::plusBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "minus", (ptrBuiltin)Nelson::OperatorsGateway::minusBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    //=============================================================================
    { "uminus", (ptrBuiltin)Nelson::OperatorsGateway::uminusBuiltin, 1, 1, CPP_BUILTIN },
    { "uplus", (ptrBuiltin)Nelson::OperatorsGateway::uplusBuiltin, 1, 1, CPP_BUILTIN },
    { "colon", (ptrBuiltin)Nelson::OperatorsGateway::colonBuiltin, 1, 2, CPP_BUILTIN },
    { "ctranspose", (ptrBuiltin)Nelson::OperatorsGateway::ctransposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "transpose", (ptrBuiltin)Nelson::OperatorsGateway::transposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "eq", (ptrBuiltin)Nelson::OperatorsGateway::eqBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ge", (ptrBuiltin)Nelson::OperatorsGateway::geBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "gt", (ptrBuiltin)Nelson::OperatorsGateway::gtBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "le", (ptrBuiltin)Nelson::OperatorsGateway::leBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "lt", (ptrBuiltin)Nelson::OperatorsGateway::ltBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ne", (ptrBuiltin)Nelson::OperatorsGateway::neBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ldivide", (ptrBuiltin)Nelson::OperatorsGateway::ldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rdivide", (ptrBuiltin)Nelson::OperatorsGateway::rdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mldivide", (ptrBuiltin)Nelson::OperatorsGateway::mldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mrdivide", (ptrBuiltin)Nelson::OperatorsGateway::mrdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mtimes", (ptrBuiltin)Nelson::OperatorsGateway::mtimesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "times", (ptrBuiltin)Nelson::OperatorsGateway::timesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "not", (ptrBuiltin)Nelson::OperatorsGateway::notBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "or", (ptrBuiltin)Nelson::OperatorsGateway::orBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "and", (ptrBuiltin)Nelson::OperatorsGateway::andBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "any", (ptrBuiltin)Nelson::OperatorsGateway::anyBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "all", (ptrBuiltin)Nelson::OperatorsGateway::allBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "power", (ptrBuiltin)Nelson::OperatorsGateway::powerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mpower", (ptrBuiltin)Nelson::OperatorsGateway::mpowerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "shortcutor", (ptrBuiltin)Nelson::OperatorsGateway::shortcutorBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "shortcutand", (ptrBuiltin)Nelson::OperatorsGateway::shortcutandBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "subsindex", (ptrBuiltin)Nelson::OperatorsGateway::subsindexBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    //"subsasgn"
    //"subsref"
    { "horzcat", (ptrBuiltin)Nelson::OperatorsGateway::horzcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "vertcat", (ptrBuiltin)Nelson::OperatorsGateway::vertcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ismember", (ptrBuiltin)Nelson::OperatorsGateway::ismemberBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bitand", (ptrBuiltin)Nelson::OperatorsGateway::bitandBuiltin, 1, 2, CPP_BUILTIN },
    { "bitor", (ptrBuiltin)Nelson::OperatorsGateway::bitorBuiltin, 1, 2, CPP_BUILTIN },
    { "bitxor", (ptrBuiltin)Nelson::OperatorsGateway::bitxorBuiltin, 1, 2, CPP_BUILTIN },
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
