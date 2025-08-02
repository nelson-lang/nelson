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
#include "XmlDocExampleItem.hpp"
#include "XmlDocGenericItem.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocExamples : public XmlDocGenericItem
{
private:
    std::vector<XmlDocExampleItem*> examplesVector;
    DOCUMENT_OUTPUT outputTarget;

public:
    XmlDocExamples(DOCUMENT_OUTPUT outputTarget);
    ~XmlDocExamples();
    void
    append(const std::wstring& type, const std::wstring& description, const std::wstring& data,
        const std::wstring& imageTag);
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
    setDirectories(const std::wstring& srcDirectory, const std::wstring& dstDirectory);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
