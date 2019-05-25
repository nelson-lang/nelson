//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_TOOLS_IMPEXP QtHelpProject
{
private:
    std::wstring destdirectory;
    std::wstring filenameDestination;
    std::string utf8stream;
    std::wstring mainTitle;
    std::wstring name_space;
    std::wstring virtualfolder;

    wstringVector sectionsName;
    wstringVector sectionsUrl;

    wstringVector keywordsName;
    wstringVector keywordsUrl;

    void
    assembleContent();

    std::string
    uuid_module();

public:
    QtHelpProject(const std::wstring& destdirectory, const std::wstring& mainTitle,
        const std::wstring& name_space, const std::wstring& virtualfolder = L"help");
    ~QtHelpProject();
    std::wstring
    getFilename();
    bool
    write();
    void
    appendSection(const std::wstring& sectionName, const std::wstring& sectionUrl,
        const wstringVector& names, const wstringVector& urls);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
