//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "QtHelpProject.hpp"
#include "Types.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocMainIndex
{
private:
    std::wstring directoryDestination;
    std::wstring filenameDestination;
    std::string utf8stream;
    std::wstring mainTitle;
    std::wstring mainModuleShortName;
    bool isQtHelp;
    DOCUMENT_OUTPUT outputTarget;
    void
    htmlHeader();
    void
    htmlOpenTags();
    void
    htmlCloseTags();
    QtHelpProject* qtproject;

public:
    XmlDocMainIndex(const std::wstring& destdir, const std::wstring& mainTitle,
        const std::wstring& mainModuleShortName,
        DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocMainIndex();
    std::wstring
    getFilename();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    void
    appendSection(const std::wstring& sectionName, const std::wstring& sectionUrl,
        const wstringVector& names, const wstringVector& urls, const wstringVector& descriptions);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
