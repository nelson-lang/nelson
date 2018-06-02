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
#include "QtHelpProject.hpp"
#include "HtmlTags.hpp"
#include "RelativePath.hpp"
#include "characters_encoding.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
QtHelpProject::QtHelpProject(std::wstring destdirectory, std::wstring mainTitle,
    std::wstring name_space, std::wstring virtualfolder)
{
    this->destdirectory = destdirectory;
    this->filenameDestination = destdirectory + L"/helpproject.qhp";
    this->utf8stream = "";
    this->mainTitle = mainTitle;
    this->name_space = name_space;
    this->virtualfolder = virtualfolder;
    this->sectionsName.clear();
    this->sectionsUrl.clear();
    this->keywordsName.clear();
    this->keywordsUrl.clear();
}
//=============================================================================
QtHelpProject::~QtHelpProject()
{
    this->destdirectory = L"";
    this->filenameDestination = L"";
    this->utf8stream = "";
    this->mainTitle = L"";
    this->name_space = L"";
    this->virtualfolder = L"";
    this->sectionsName.clear();
    this->sectionsUrl.clear();
    this->keywordsName.clear();
    this->keywordsUrl.clear();
}
//=============================================================================
std::wstring
QtHelpProject::getFilename()
{
    return this->filenameDestination;
}
//=============================================================================
void
QtHelpProject::assembleContent()
{
    this->utf8stream
        = this->utf8stream + "<?xml version = \"1.0\" encoding = \"UTF-8\"?>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<QtHelpProject version = \"1.0\">" + std::string("\n");
    this->utf8stream = this->utf8stream + "<namespace>" + wstring_to_utf8(this->name_space)
        + "</namespace>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<virtualFolder>" + wstring_to_utf8(this->virtualfolder)
        + "</virtualFolder>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<customFilter name = \""
        + wstring_to_utf8(this->mainTitle) + "\">" + std::string("\n");
    this->utf8stream = this->utf8stream + "<filterAttribute>" + wstring_to_utf8(this->mainTitle)
        + "</filterAttribute>" + std::string("\n");
    this->utf8stream = this->utf8stream + "</customFilter>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<filterSection>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<filterAttribute>" + wstring_to_utf8(this->mainTitle)
        + "</filterAttribute>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<toc>" + std::string("\n");
    if (sectionsName.size() > 1) {
        this->utf8stream = this->utf8stream + "<section title = \""
            + wstring_to_utf8(this->mainTitle) + "\" ref = \"" + "index.html" + "\">"
            + std::string("\n");
        for (size_t k = 0; k < this->sectionsName.size(); k++) {
            bool bSuccess;
            std::wstring relativepath
                = RelativePath(this->destdirectory, this->sectionsUrl[k], bSuccess);
            this->utf8stream = this->utf8stream + "\t<section title = \""
                + wstring_to_utf8(this->sectionsName[k]) + "\" ref = \""
                + wstring_to_utf8(relativepath) + "\"/>" + std::string("\n");
        }
        this->utf8stream = this->utf8stream + "</section>" + std::string("\n");
    } else {
        bool bSuccess;
        std::wstring relativepath
            = RelativePath(this->destdirectory, this->sectionsUrl[0], bSuccess);
        this->utf8stream = this->utf8stream + "<section title = \""
            + wstring_to_utf8(this->sectionsName[0]) + "\" ref = \"" + wstring_to_utf8(relativepath)
            + "\"/>" + std::string("\n");
    }
    this->utf8stream = this->utf8stream + "</toc>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<keywords>" + std::string("\n");
    for (size_t k = 0; k < this->keywordsName.size(); k++) {
        this->utf8stream = this->utf8stream + "<keyword name = \""
            + wstring_to_utf8(this->keywordsName[k]) + "\" id = \""
            + wstring_to_utf8(this->mainTitle) + "::" + wstring_to_utf8(this->keywordsName[k])
            + "\" ref = \"" + wstring_to_utf8(this->keywordsUrl[k]) + "\"/>" + std::string("\n");
    }
    this->utf8stream = this->utf8stream + "</keywords>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<files>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.css</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.js</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.html</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.png</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.svg</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.gif</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.jpg</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "<file>*.jpeg</file>" + std::string("\n");
    this->utf8stream = this->utf8stream + "</files>" + std::string("\n");
    this->utf8stream = this->utf8stream + "</filterSection>" + std::string("\n");
    this->utf8stream = this->utf8stream + "</QtHelpProject>" + std::string("\n");
    this->utf8stream = this->utf8stream + std::string("\n");
}
//=============================================================================
std::string
QtHelpProject::uuid_module()
{
    boost::uuids::random_generator generator;
    boost::uuids::uuid uuid = generator();
    return boost::uuids::to_string(uuid);
}
//=============================================================================
bool
QtHelpProject::write()
{
    bool res = false;
    std::ofstream destfile;
    std::string destUtf8Stream = "";
    this->assembleContent();
    if (!this->utf8stream.empty()) {
        destUtf8Stream = this->utf8stream;
    }
    res = !destUtf8Stream.empty();
    if (res) {
#if _MSC_VER
        destfile.open(this->filenameDestination);
#else
        destfile.open(wstring_to_utf8(this->filenameDestination));
#endif
        if (destfile.is_open()) {
            destfile << destUtf8Stream << std::endl;
            destfile.close();
            res = true;
        } else {
            res = false;
        }
    }
    return res;
}
//=============================================================================
void
QtHelpProject::appendSection(
    std::wstring sectionName, std::wstring sectionUrl, wstringVector names, wstringVector urls)
{
    this->sectionsName.push_back(sectionName);
    this->sectionsUrl.push_back(sectionUrl);
    for (size_t k = 0; k < names.size(); k++) {
        this->keywordsName.push_back(names[k]);
    }
    for (size_t k = 0; k < urls.size(); k++) {
        this->keywordsUrl.push_back(urls[k]);
    }
}
//=============================================================================
}
//=============================================================================
