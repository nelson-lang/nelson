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
#include "XmlDocDirectory.hpp"
#include "Messages.hpp"
#include "RelativePath.hpp"
#include "Types.hpp"
#include "XmlDocChapterItem.hpp"
#include "i18n.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocDirectory::XmlDocDirectory(std::wstring srcDirectory, std::wstring dstDirectory,
    bool bOverwriteExistingFiles, DOCUMENT_OUTPUT outputTarget)
{
    this->sectionUpName = L"";
    this->sectionUpUrl = L"";
    this->srcDirectory = srcDirectory;
    this->dstDirectory = dstDirectory;
    this->outputTarget = outputTarget;
    boost::filesystem::directory_iterator end_iter;
    wstringVector listXmlFiles;
    for (boost::filesystem::directory_iterator dir_iter(this->srcDirectory); dir_iter != end_iter;
         ++dir_iter) {
        boost::filesystem::path current = dir_iter->path();
        if (boost::iequals(current.extension().generic_wstring(), ".xml")) {
            listXmlFiles.push_back(current.generic_wstring());
        }
    }
    if (!listXmlFiles.empty()) {
        this->xmlDocFiles = new XmlDocListOfFiles(
            listXmlFiles, this->dstDirectory, bOverwriteExistingFiles, this->outputTarget);
    }
}
//=============================================================================
bool
XmlDocDirectory::read()
{
    if (this->xmlDocFiles) {
        this->xmlDocFiles->setUpSection(this->sectionUpName, this->sectionUpUrl);
        return this->xmlDocFiles->read();
    }
    return false;
}
//=============================================================================
XmlDocDirectory::~XmlDocDirectory()
{
    this->sectionUpName = L"";
    this->sectionUpUrl = L"";
    this->srcDirectory = L"";
    this->dstDirectory = L"";
    if (this->xmlDocFiles) {
        delete this->xmlDocFiles;
        this->xmlDocFiles = nullptr;
    }
}
//=============================================================================
bool
XmlDocDirectory::writeAsMarkdown()
{
    if (this->xmlDocFiles) {
        this->xmlDocFiles->setUpSection(this->sectionUpName, this->sectionUpUrl);
        return this->xmlDocFiles->writeAsMarkdown();
    }
    return false;
}
//=============================================================================
bool
XmlDocDirectory::writeAsHtml()
{
    if (this->xmlDocFiles) {
        this->xmlDocFiles->setUpSection(this->sectionUpName, this->sectionUpUrl);
        return this->xmlDocFiles->writeAsHtml();
    }
    return false;
}
//=============================================================================
std::wstring
XmlDocDirectory::getLastError()
{
    if (this->xmlDocFiles) {
        return this->xmlDocFiles->getLastError();
    }
    return _W("XmlDocDirectory not created.");
}
//=============================================================================
std::wstring
XmlDocDirectory::getGeneratedChapterFilename()
{
    if (this->xmlDocFiles) {
        return this->xmlDocFiles->getGeneratedChapterFilename();
    }
    return L"";
}
//=============================================================================
std::wstring
XmlDocDirectory::getChapterTitle()
{
    if (this->xmlDocFiles) {
        return this->xmlDocFiles->getChapterTitle();
    }
    return L"";
}
//=============================================================================
void
XmlDocDirectory::getIndex(wstringVector& names, wstringVector& urls, wstringVector& descriptions)
{
    if (this->xmlDocFiles) {
        this->xmlDocFiles->getIndex(names, urls, descriptions);
    }
}
//=============================================================================
void
XmlDocDirectory::setUpSection(std::wstring sectionName, std::wstring sectionUrl)
{
    this->sectionUpName = sectionName;
    this->sectionUpUrl = sectionUrl;
}
//=============================================================================
std::wstring
XmlDocDirectory::getModuleName()
{
    if (this->xmlDocFiles) {
        return this->xmlDocFiles->getModuleName();
    }
    return L"";
}
//=============================================================================
}
//=============================================================================
