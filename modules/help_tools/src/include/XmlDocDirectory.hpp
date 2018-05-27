//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "XmlDocDocument.hpp"
#include "XmlDocListOfFiles.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocDirectory
{

private:
    std::wstring srcDirectory;
    std::wstring dstDirectory;
    XmlDocListOfFiles* xmlDocFiles;
    DOCUMENT_OUTPUT outputTarget;
    std::wstring sectionUpName;
    std::wstring sectionUpUrl;

public:
    XmlDocDirectory(std::wstring srcDirectory, std::wstring dstDirectory,
        bool bOverwriteExistingFiles = false, DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocDirectory();
    bool
    read();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    std::wstring
    getLastError();
    std::wstring
    getGeneratedChapterFilename();
    std::wstring
    getChapterTitle();
    std::wstring
    getModuleName();
    void
    setUpSection(std::wstring sectionName, std::wstring sectionUrl);
    void
    getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
