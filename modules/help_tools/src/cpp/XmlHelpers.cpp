//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
addRawXml(xmlNodePtr parent, const std::string& rawxml)
{

    xmlNodePtr list = nullptr;

    int ret = xmlParseInNodeContext(parent, rawxml.c_str(), (int)rawxml.size(), 0, &list);

    if (ret == 0 && list) {
        for (xmlNodePtr cur = list; cur; cur = cur->next) {
            xmlAddChild(parent, xmlCopyNode(cur, 1));
        }
        xmlFreeNodeList(list);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
