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
#include "XmlDocChapterRefItem.hpp"
#include "XmlDocGenericItem.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocChapterIndexItem : public XmlDocGenericItem
{
private:
    std::vector<XmlDocChapterRefItem*> chapterRefVector;

public:
    XmlDocChapterIndexItem();
    ~XmlDocChapterIndexItem();
    std::wstring
    getItemType();
    bool
    append(
        const std::wstring& linkname, const std::wstring& linkurl, const std::wstring& description);
    bool
    writeAsHtml(std::string& utf8stream);
    bool
    writeAsMarkdown(std::string& utf8stream);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
