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
#include "CleanupHelptools.hpp"
#include "headcommentsBuiltin.hpp"
#include "htmltopdfBuiltin.hpp"
#include "markdownBuiltin.hpp"
#include "xmldocbuildBuiltin.hpp"
#include "xmldoccheckerBuiltin.hpp"
#include "docrootBuiltin.hpp"
#include "xmltransformBuiltin.hpp"
#include "xmlprettyprintBuiltin.hpp"
#include "xmldocmergesummaryBuiltin.hpp"
#include "__xmldocgenerateimages__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"help_tools";
//=============================================================================
static const nlsGateway gateway[] = { { "headcomments",
                                          (ptrBuiltin)Nelson::HelpToolsGateway::headcommentsBuiltin,
                                          1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "xmldocchecker", (ptrBuiltin)Nelson::HelpToolsGateway::xmldoccheckerBuiltin, 2, 1,
        CPP_BUILTIN },
    { "htmltopdf", (ptrBuiltin)Nelson::HelpToolsGateway::htmltopdfBuiltin, 0, 2, CPP_BUILTIN },
    { "markdown", (ptrBuiltin)Nelson::HelpToolsGateway::markdownBuiltin, 1, -2 },
    { "xmldocbuild", (ptrBuiltin)Nelson::HelpToolsGateway::xmldocbuildBuiltin, -1, 5 },
    { "docroot", (ptrBuiltin)Nelson::HelpToolsGateway::docrootBuiltin, 1, 1 },
    { "xmldocmergesummary", (ptrBuiltin)Nelson::HelpToolsGateway::xmldocmergesummaryBuiltin, -1,
        2 },
    { "xmlprettyprint", (ptrBuiltin)Nelson::HelpToolsGateway::xmlprettyprintBuiltin, 1, -1 },
    { "xmltransform", (ptrBuiltin)Nelson::HelpToolsGateway::xmltransformBuiltin, -1, 2 },
    { "__xmldocgenerateimages__",
        (ptrBuiltin)Nelson::HelpToolsGateway::__xmlgenerateimages__Builtin, 3, 2, CPP_BUILTIN } };
//=============================================================================
static bool
finishHelpToolsModule(Nelson::Evaluator* eval)
{
    return cleanupHelptools();
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishHelpToolsModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
