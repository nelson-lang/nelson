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
#include "Operators.hpp"
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
#include "subsasgnBuiltin.hpp"
#include "subsrefBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"operators";
//=============================================================================
static const nlsGateway gateway[] = {
    /* operators */
    { PLUS_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::plusBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { MINUS_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::minusBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { UMINUS_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::uminusBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { UPLUS_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::uplusBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { AND_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::andBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { COLON_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::colonBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { CTRANSPOSE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::ctransposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { TRANSPOSE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::transposeBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { EQ_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::eqBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { GE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::geBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { GT_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::gtBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { LE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::leBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { LT_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::ltBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { NE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::neBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { LDIVIDE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::ldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { RDIVIDE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::rdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { MLDIVIDE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::mldivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { MRDIVIDE_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::mrdivideBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { MTIMES_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::mtimesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { TIMES_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::timesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { NOT_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::notBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OR_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::orBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { POWER_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::powerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { MPOWER_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::mpowerBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { SUBSASGN_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::subsindexBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { HORZCAT_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::horzcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { VERTCAT_OPERATOR_STR, (ptrBuiltin)Nelson::OperatorsGateway::vertcatBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "subsasgn", (ptrBuiltin)Nelson::OperatorsGateway::subsasgnBuiltin, 1, 3 },
    { "subsref", (ptrBuiltin)Nelson::OperatorsGateway::subsrefBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ismember", (ptrBuiltin)Nelson::OperatorsGateway::ismemberBuiltin, 1, 2 },
    { "bitand", (ptrBuiltin)Nelson::OperatorsGateway::bitandBuiltin, 1, 2, CPP_BUILTIN },
    { "bitor", (ptrBuiltin)Nelson::OperatorsGateway::bitorBuiltin, 1, 2, CPP_BUILTIN },
    { "bitxor", (ptrBuiltin)Nelson::OperatorsGateway::bitxorBuiltin, 1, 2, CPP_BUILTIN },
    { "any", (ptrBuiltin)Nelson::OperatorsGateway::anyBuiltin, 1, 2 },
    { "all", (ptrBuiltin)Nelson::OperatorsGateway::allBuiltin, 1, 2, CPP_BUILTIN },
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
