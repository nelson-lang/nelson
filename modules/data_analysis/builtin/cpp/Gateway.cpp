//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "uniqueBuiltin.hpp"
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
    { "max", (ptrBuiltin)Nelson::DataAnalysisGateway::maxBuiltin, 2, 4, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { "min", (ptrBuiltin)Nelson::DataAnalysisGateway::minBuiltin, 2, 4, CPP_BUILTIN_WITH_EVALUATOR,
        NLS_OVERLOAD_AUTO_OFF },
    { "prod", (ptrBuiltin)Nelson::DataAnalysisGateway::prodBuiltin, 1, 3 },
    { "sum", (ptrBuiltin)Nelson::DataAnalysisGateway::sumBuiltin, 1, 3 },
    { "cumsum", (ptrBuiltin)Nelson::DataAnalysisGateway::cumsumBuiltin, 1, 4 },
    { "cumprod", (ptrBuiltin)Nelson::DataAnalysisGateway::cumprodBuiltin, 1, 4 },
    { "ismissing", (ptrBuiltin)Nelson::DataAnalysisGateway::ismissingBuiltin, 1, 1 },
    { "sort", (ptrBuiltin)Nelson::DataAnalysisGateway::sortBuiltin, 2, -1 },
    { "conv2", (ptrBuiltin)Nelson::DataAnalysisGateway::conv2Builtin, 1, 4 },
    { "unique", (ptrBuiltin)Nelson::DataAnalysisGateway::uniqueBuiltin, -1, -1, CPP_BUILTIN },
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
