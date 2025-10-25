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
#include "CleanupXml.hpp"
#include "xmlcheckerBuiltin.hpp"
#include "xmltransformBuiltin.hpp"
#include "xmlprettyprintBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"xml";
//=============================================================================
static const nlsGateway gateway[] = {
    { "xmlchecker", (ptrBuiltin)Nelson::XmlGateway::xmlcheckerBuiltin, -1, 2, CPP_BUILTIN },
    { "xmlprettyprint", (ptrBuiltin)Nelson::XmlGateway::xmlprettyprintBuiltin, 0, -1, CPP_BUILTIN },
    { "xmltransform", (ptrBuiltin)Nelson::XmlGateway::xmltransformBuiltin, -1, -3, CPP_BUILTIN },
};
//=============================================================================
static bool
finishXmlModule(Nelson::Evaluator* eval)
{
    return cleanupXml();
}
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishXmlModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
