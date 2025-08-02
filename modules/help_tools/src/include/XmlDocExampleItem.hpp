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
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocExampleItem : public XmlDocGenericItem
{
private:
    std::wstring _type;
    std::wstring _description;
    std::wstring _data;
    std::wstring _imageTag;
    DOCUMENT_OUTPUT _outputTarget;
    std::wstring _srcDirectory;
    std::wstring _dstDirectory;
    std::wstring _imageSource;
    std::wstring _imageDestination;

public:
    XmlDocExampleItem(const std::wstring& type, const std::wstring& description,
        const std::wstring& data, const std::wstring& imageTag, DOCUMENT_OUTPUT outputTarget);
    ~XmlDocExampleItem();
    std::wstring
    getType();
    std::wstring
    getDescription();
    std::wstring
    getData();
    std::wstring
    getImageTag();
    std::wstring
    getItemType();
    bool
    isNelsonExample();
    bool
    writeAsHtml(std::string& utf8stream);
    bool
    writeHeaderAsHtml(std::string& utf8stream);
    bool
    writeAsMarkdown(std::string& utf8stream);
    bool
    writeHeaderAsMarkdown(std::string& utf8stream);
    void
    setDirectories(const std::wstring& srcDirectory, const std::wstring& dstDirectory);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
