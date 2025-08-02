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
#include "Types.hpp"
#include "XmlDocGenericItem.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocDescriptionItem : public XmlDocGenericItem
{
private:
    std::wstring _description;
    bool haveImages;

    wstringVector imagesTag;
    wstringVector imagesSource;
    wstringVector imagesDestination;
    std::wstring srcDirectory;
    std::wstring destDirectory;

    bool
    checkImageTag();
    void
    replaceImageTag();

public:
    XmlDocDescriptionItem(const std::wstring& description);
    ~XmlDocDescriptionItem();
    void
    setValue(const std::wstring& value);
    std::wstring
    getValue();
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

    void
    setDirectories(const std::wstring& srcDirectory, const std::wstring& destDirectory);
    void
    searchImageTag();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
