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
