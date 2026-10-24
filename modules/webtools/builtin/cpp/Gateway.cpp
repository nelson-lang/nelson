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
// nlsBuildConfig.h defines WITH_LIBCURL / WITH_LIBGIT2 as 0 or 1; the
// WebAssembly build provides webREST without libcurl (NELSON_WEBTOOLS_WASM).
#if WITH_LIBCURL || WITH_LIBGIT2 || defined(NELSON_WEBTOOLS_WASM)
static const nlsGateway gateway[] = {
#if WITH_LIBCURL || defined(NELSON_WEBTOOLS_WASM)
    { "webREST", (ptrBuiltin)Nelson::WebtoolsGateway::webRESTBuiltin, 1, 5,
        CPP_BUILTIN_WITH_EVALUATOR },
#endif
#if WITH_LIBGIT2
    { "repo", (ptrBuiltin)Nelson::WebtoolsGateway::repoBuiltin, 1, 2 },
#endif
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
NLS_GATEWAY_REMOVE(gateway)
#endif
//=============================================================================
