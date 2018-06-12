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
    XmlDocDescriptionItem(std::wstring description);
    ~XmlDocDescriptionItem();
    void
    setValue(std::wstring value);
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
    setDirectories(std::wstring srcDirectory, std::wstring destDirectory);
    void
    searchImageTag();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
