//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include "XmlDocMainIndex.hpp"
#include "HtmlTags.hpp"
#include "RelativePath.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
XmlDocMainIndex::XmlDocMainIndex(const std::wstring& destdir, const std::wstring& _mainTitle,
    const std::wstring& mainModuleShortName, DOCUMENT_OUTPUT outputTarget)
{
    this->directoryDestination.append(destdir);
    if (outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
        this->filenameDestination = destdir + L"/SUMMARY.md";
    } else {
        this->filenameDestination = destdir + L"/index.html";
    }
    this->utf8stream.clear();
    this->mainTitle.assign(_mainTitle);
    this->mainModuleShortName.assign(mainModuleShortName);
    this->outputTarget = outputTarget;
    if (outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
        this->utf8stream
            = this->utf8stream + "* [" + wstring_to_utf8(this->mainTitle) + "](README.md)\n";
    } else {
        this->htmlHeader();
        this->htmlOpenTags();
    }
    if (outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
        std::wstring name_space = NelsonConfiguration::getInstance()->getNelsonApplicationId()
            + std::wstring(L".modules.") + mainModuleShortName + std::wstring(L".help");
        this->qtproject = new QtHelpProject(this->directoryDestination, mainTitle, name_space);
    } else {
        this->qtproject = nullptr;
    }
}
//=============================================================================
XmlDocMainIndex::~XmlDocMainIndex()
{
    this->directoryDestination.clear();
    this->filenameDestination.clear();
    this->utf8stream.clear();
    this->mainTitle.clear();
    this->mainModuleShortName.clear();
    this->outputTarget = DOCUMENT_OUTPUT::HMTL;
    if (this->qtproject) {
        delete this->qtproject;
    }
    this->qtproject = nullptr;
}
//=============================================================================
void
XmlDocMainIndex::htmlHeader()
{
    this->utf8stream = this->utf8stream + HTML_DOCTYPE_HTML_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_HEAD_IN_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_HTML_IN_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_GENERATOR_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_TITLE_IN_TAG + wstring_to_utf8(this->mainTitle)
        + HTML_TITLE_OUT_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_HEAD_OUT_TAG + std::string("\n");
}
//=============================================================================
void
XmlDocMainIndex::htmlOpenTags()
{
    this->utf8stream = this->utf8stream + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_BODY_IN_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_BODY_IN_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + "<h1 class = \"refname\">"
        + wstring_to_utf8(this->mainTitle) + HTML_H1_OUT_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_HR_OUT_TAG + std::string("\n");
}
//=============================================================================
void
XmlDocMainIndex::htmlCloseTags()
{
    this->utf8stream = this->utf8stream + HTML_HR_OUT_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_BODY_OUT_TAG + std::string("\n");
    this->utf8stream = this->utf8stream + HTML_HTML_OUT_TAG + std::string("\n");
}
//=============================================================================
std::wstring
XmlDocMainIndex::getFilename()
{
    return this->filenameDestination;
}
//=============================================================================
bool
XmlDocMainIndex::writeAsHtml()
{
    std::ofstream htmlfile;
    std::string htmlUtf8Stream = "";
    if (!this->utf8stream.empty()) {
        this->htmlCloseTags();
        htmlUtf8Stream = this->utf8stream;
    }
    bool res = !htmlUtf8Stream.empty();
    if (res) {
#if _MSC_VER
        htmlfile.open(this->filenameDestination);
#else
        htmlfile.open(wstring_to_utf8(this->filenameDestination));
#endif
        if (htmlfile.is_open()) {
            htmlfile << htmlUtf8Stream << std::endl;
            htmlfile.close();
            res = true;
        } else {
            res = false;
        }
    }
    if (res) {
        if (this->outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
            if (this->qtproject) {
                res = this->qtproject->write();
            }
        }
    }
    return res;
}
//=============================================================================
bool
XmlDocMainIndex::writeAsMarkdown()
{
    std::ofstream markdownfile;
    std::string mdUtf8Stream = "";
    if (!this->utf8stream.empty()) {
        mdUtf8Stream = this->utf8stream;
    }
    bool res = !mdUtf8Stream.empty();
    if (res) {
#if _MSC_VER
        markdownfile.open(this->filenameDestination);
#else
        markdownfile.open(wstring_to_utf8(this->filenameDestination));
#endif
        if (markdownfile.is_open()) {
            markdownfile << mdUtf8Stream << std::endl;
            markdownfile.close();
            res = true;
        } else {
            res = false;
        }
    }
    return res;
}
//=============================================================================
void
XmlDocMainIndex::appendSection(const std::wstring& sectionName, const std::wstring& sectionUrl,
    const wstringVector& names, const wstringVector& urls, const wstringVector& descriptions)
{
    if (this->outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
        bool bSuccess;
        std::wstring relative = RelativePath(this->directoryDestination, sectionUrl, bSuccess);
        if ((names.size() == urls.size()) && (names.size() == descriptions.size())) {
            for (size_t k = 0; k < urls.size(); k++) {
                relative = RelativePath(this->directoryDestination, urls[k], bSuccess);
                this->utf8stream = this->utf8stream + "    * " + "[" + wstring_to_utf8(names[k])
                    + "]" + "(" + wstring_to_utf8(relative) + ")" + "\n";
            }
        }
    } else {
        this->utf8stream = this->utf8stream + std::string("\n");
        this->utf8stream = this->utf8stream + HTML_DIV_IN_TAG + std::string("\n");
        this->utf8stream = this->utf8stream + HTML_UL_IN_TAG + std::string("\n");
        bool bSuccess;
        std::wstring relative = RelativePath(this->directoryDestination, sectionUrl, bSuccess);
        this->utf8stream = this->utf8stream + HTML_LI_IN_TAG + "<a href = \""
            + wstring_to_utf8(relative) + R"(" class = "chapter">)" + wstring_to_utf8(sectionName)
            + HTML_A_OUT_TAG + HTML_LI_OUT_TAG + std::string("\n");
        this->utf8stream = this->utf8stream + "<ul class = \"list-chapter\">" + std::string("\n");
        if ((names.size() == urls.size()) && (names.size() == descriptions.size())) {
            for (size_t k = 0; k < urls.size(); k++) {
                relative = RelativePath(this->directoryDestination, urls[k], bSuccess);
                this->utf8stream = this->utf8stream + HTML_LI_IN_TAG
                    + "<a href = " + wstring_to_utf8(relative) + " class = \"refentry\">"
                    + wstring_to_utf8(names[k]) + HTML_A_OUT_TAG
                    + "&mdash; <span class = \"refentry-description\">"
                    + wstring_to_utf8(descriptions[k]) + HTML_SPAN_OUT_TAG + HTML_LI_OUT_TAG
                    + std::string("\n");
            }
        }
        this->utf8stream = this->utf8stream + HTML_UL_OUT_TAG + std::string("\n");
        this->utf8stream = this->utf8stream + HTML_UL_OUT_TAG + std::string("\n");
        this->utf8stream = this->utf8stream + HTML_DIV_OUT_TAG + std::string("\n");
        if (this->outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
            if (this->qtproject) {
                this->qtproject->appendSection(sectionName, sectionUrl, names, urls);
            }
        }
    }
}
//=============================================================================
}
//=============================================================================
