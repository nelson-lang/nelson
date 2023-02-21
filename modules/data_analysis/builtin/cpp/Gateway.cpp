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
#include "prodBuiltin.hpp"
#include "sumBuiltin.hpp"
#include "ismissingBuiltin.hpp"
#include "sortBuiltin.hpp"
#include "maxBuiltin.hpp"
#include "minBuiltin.hpp"
#include "conv2Builtin.hpp"
#include "cumsumBuiltin.hpp"
#include "cumprodBuiltin.hpp"
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
    { "cumsum", (ptrBuiltin)Nelson::DataAnalysisGateway::cumsumBuiltin, 1, 4,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cumprod", (ptrBuiltin)Nelson::DataAnalysisGateway::cumprodBuiltin, 1, 4,
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
