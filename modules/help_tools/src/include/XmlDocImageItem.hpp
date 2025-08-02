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
class NLSHELP_TOOLS_IMPEXP XmlDocImageItem : public XmlDocGenericItem
{
private:
    std::wstring tag;
    std::wstring srcDirectory;
    std::wstring destDirectory;
    std::wstring imageSource;
    std::wstring imageDestination;

public:
    XmlDocImageItem(const std::wstring& tag);
    ~XmlDocImageItem();
    std::wstring
    getItemType() override;
    bool
    writeAsHtml(std::string& utf8stream) override;
    bool
    writeAsMarkdown(std::string& utf8stream) override;
    void
    setDirectories(const std::wstring& srcDirectory, const std::wstring& destDirectory);
    void
    findImage();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
