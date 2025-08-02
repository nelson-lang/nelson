//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "XmlDocGenericItem.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocLinkItem : public XmlDocGenericItem
{
private:
    std::wstring _name;
    std::wstring _link;

public:
    XmlDocLinkItem(const std::wstring& name, const std::wstring& link);
    ~XmlDocLinkItem();
    std::wstring
    getName();
    std::wstring
    getLink();
    std::wstring
    getItemType();
    bool
    writeAsHtml(std::string& utf8stream);
    bool
    writeAsMarkdown(std::string& utf8stream);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
