//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "webRESTBuiltin.hpp"
#include "repoBuiltin.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"webtools";
//=============================================================================
#if defined(WITH_LIBCURL) || defined(WITH_LIBGIT2)
static const nlsGateway gateway[] = {
#ifdef WITH_LIBCURL
    { "webREST", (ptrBuiltin)Nelson::WebtoolsGateway::webRESTBuiltin, 1, 5,
        CPP_BUILTIN_WITH_EVALUATOR },
#endif
#ifdef WITH_LIBGIT2
    { "repo", (ptrBuiltin)Nelson::WebtoolsGateway::repoBuiltin, 1, 2 },
#endif
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
NLSGATEWAYINFO(gateway)
NLSGATEWAYREMOVE(gateway)
NLSGATEWAYNAME()
#endif
//=============================================================================
