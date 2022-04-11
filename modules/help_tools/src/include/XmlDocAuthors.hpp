//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "XmlDocAuthorItem.hpp"
#include "XmlDocGenericItem.hpp"
#include "nlsHelp_tools_exports.h"
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocAuthors : public XmlDocGenericItem
{
private:
    boost::container::vector<XmlDocAuthorItem*> authorVector;

public:
    XmlDocAuthors();
    ~XmlDocAuthors();
    void
    append(const std::wstring& value);
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
