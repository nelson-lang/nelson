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
#include "xmlreadBuiltin.hpp"
#include "xmlwriteBuiltin.hpp"
#include "xsltBuiltin.hpp"
#include "readstructBuiltin.hpp"
#include "writestructBuiltin.hpp"
#include "xmlDomMethodBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"xml";
//=============================================================================
static const nlsGateway gateway[] = {
    { "readstruct", (ptrBuiltin)Nelson::XmlGateway::readstructBuiltin, 1, -2, CPP_BUILTIN },
    { "writestruct", (ptrBuiltin)Nelson::XmlGateway::writestructBuiltin, 0, -3, CPP_BUILTIN },
    { "xmlchecker", (ptrBuiltin)Nelson::XmlGateway::xmlcheckerBuiltin, -1, 2, CPP_BUILTIN },
    { "xmlprettyprint", (ptrBuiltin)Nelson::XmlGateway::xmlprettyprintBuiltin, 0, -1, CPP_BUILTIN },
    { "xmlread", (ptrBuiltin)Nelson::XmlGateway::xmlreadBuiltin, 2, -2, CPP_BUILTIN },
    { "xmlwrite", (ptrBuiltin)Nelson::XmlGateway::xmlwriteBuiltin, -1, -1, CPP_BUILTIN },
    { "__xml_dom_method__", (ptrBuiltin)Nelson::XmlGateway::xmlDomMethodBuiltin, 1, -1,
        CPP_BUILTIN },
    { "xmltransform", (ptrBuiltin)Nelson::XmlGateway::xmltransformBuiltin, -1, -3, CPP_BUILTIN },
    { "xslt", (ptrBuiltin)Nelson::XmlGateway::xsltBuiltin, -1, -1, CPP_BUILTIN },
};
//=============================================================================
static bool
finishXmlModule(Nelson::Evaluator* eval)
{
    return cleanupXml();
}
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishXmlModule)
//=============================================================================
