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
#include "nlsHelp_tools_exports.h"
#include <fstream>
#include <iostream>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocGenericItem
{
public:
    virtual std::wstring
    getItemType()
        = 0;
    virtual bool
    writeAsHtml(std::string& utf8stream)
        = 0;
    virtual bool
    writeAsMarkdown(std::string& utf8stream)
        = 0;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
