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
#include "Types.hpp"
#include "XmlDocDirectory.hpp"
#include "XmlDocDocument.hpp"
#include "XmlDocMainIndex.hpp"
#include "XmlTarget.hpp"
#include "nlsHelp_tools_exports.h"
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP XmlDocListOfDirectories
{

private:
    wstringVector srcDirectories;
    std::wstring dstDirectory;
    std::wstring mainTitle;
    boost::container::vector<XmlDocDirectory*> itemsDirectories;
    bool bOverwriteExistingFiles;
    std::wstring lastError;
    void
    clearItems();
    DOCUMENT_OUTPUT outputTarget;
    XmlDocMainIndex* mainIndex;

public:
    XmlDocListOfDirectories(wstringVector srcDirectories, std::wstring dstDirectory,
        std::wstring mainTitle, bool bOverwriteExistingFiles = false,
        DOCUMENT_OUTPUT outputTarget = DOCUMENT_OUTPUT::HMTL);
    ~XmlDocListOfDirectories();
    bool
    read();
    bool
    writeAsHtml();
    bool
    writeAsMarkdown();
    std::wstring
    getLastError();
    void
    getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions);
    std::wstring
    getOutputHelpBasename();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
