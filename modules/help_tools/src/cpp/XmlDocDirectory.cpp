//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "XmlDocDirectory.hpp"
#include "PredefinedErrorMessages.hpp"
#include "RelativePath.hpp"
#include "Types.hpp"
#include "XmlDocChapterItem.hpp"
#include "i18n.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocDirectory::XmlDocDirectory(const std::wstring& _srcDirectory,
    const std::wstring& dstDirectory, bool bOverwriteExistingFiles, DOCUMENT_OUTPUT outputTarget)
{
    this->sectionUpName.clear();
    this->sectionUpUrl.clear();
    this->srcDirectory.assign(_srcDirectory);
    this->dstDirectory.assign(dstDirectory);
    this->outputTarget = outputTarget;
    nfs::directory_iterator end_iter;
    wstringVector listXmlFiles;
    for (nfs::directory_iterator dir_iter(this->srcDirectory); dir_iter != end_iter; ++dir_iter) {
        nfs::path current = dir_iter->path();
        if (StringHelpers::iequals(current.extension().generic_wstring(), L".xml")) {
            listXmlFiles.push_back(current.generic_wstring());
        }
    }
    if (!listXmlFiles.empty()) {
        std::sort(listXmlFiles.begin(), listXmlFiles.end());
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
    this->sectionUpName.clear();
    this->sectionUpUrl.clear();
    this->srcDirectory.clear();
    this->dstDirectory.clear();
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
XmlDocDirectory::setUpSection(const std::wstring& sectionName, const std::wstring& sectionUrl)
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
