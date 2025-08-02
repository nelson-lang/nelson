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
#include "XmlDocHistoryItem.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocHistory : public XmlDocGenericItem
{
private:
    std::vector<XmlDocHistoryItem*> historyVector;

public:
    XmlDocHistory();
    ~XmlDocHistory();
    void
    append(const std::wstring& version, const std::wstring& description);
    std::wstring
    getItemType();

    bool
    writeAsHtml(std::string& utf8stream);
    bool
    writeHeaderAsHtml(std::string& utf8stream);

    bool
    writeAsMarkdown(std::string& utf8stream);
    bool
    writeHeaderAsMarkdown(std::string& utf8stream);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
