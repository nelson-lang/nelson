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
#include "headcommentsBuiltin.hpp"
#include "htmltopdfBuiltin.hpp"
#include "markdownBuiltin.hpp"
#include "xmldocbuildBuiltin.hpp"
#include "docrootBuiltin.hpp"
#include "xmldocmergesummaryBuiltin.hpp"
#include "__xmldocgenerateimages__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"help_tools";
//=============================================================================
static const nlsGateway gateway[] = {
    { "htmltopdf", (ptrBuiltin)Nelson::HelpToolsGateway::htmltopdfBuiltin, 0, 2, CPP_BUILTIN },
    { "markdown", (ptrBuiltin)Nelson::HelpToolsGateway::markdownBuiltin, 1, -2 },
    { "xmldocbuild", (ptrBuiltin)Nelson::HelpToolsGateway::xmldocbuildBuiltin, -1, 5 },
    { "docroot", (ptrBuiltin)Nelson::HelpToolsGateway::docrootBuiltin, 1, 1 },
    { "xmldocmergesummary", (ptrBuiltin)Nelson::HelpToolsGateway::xmldocmergesummaryBuiltin, -1,
        2 },
    { "headcomments", (ptrBuiltin)Nelson::HelpToolsGateway::headcommentsBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__xmldocgenerateimages__",
        (ptrBuiltin)Nelson::HelpToolsGateway::__xmlgenerateimages__Builtin, 3, 2, CPP_BUILTIN }
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
