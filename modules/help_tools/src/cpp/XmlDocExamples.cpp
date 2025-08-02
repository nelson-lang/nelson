//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocExamples.hpp"
#include "HtmlTags.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
XmlDocExamples::XmlDocExamples(DOCUMENT_OUTPUT outputTarget)
{
    this->outputTarget = outputTarget;
    examplesVector.clear();
}
//=============================================================================
XmlDocExamples::~XmlDocExamples()
{
    for (auto& k : examplesVector) {
        delete k;
        k = nullptr;
    }
    examplesVector.clear();
    this->outputTarget = DOCUMENT_OUTPUT::HMTL;
}
//=============================================================================
void
XmlDocExamples::append(const std::wstring& type, const std::wstring& description,
    const std::wstring& data, const std::wstring& imageTag)
{
    XmlDocExampleItem* item = nullptr;
    try {
        item = new XmlDocExampleItem(type, description, data, imageTag, this->outputTarget);
    } catch (const std::bad_alloc&) {
        item = nullptr;
    }
    if (item) {
        examplesVector.push_back(item);
    }
}
//=============================================================================
std::wstring
XmlDocExamples::getItemType()
{
    return utf8_to_wstring(EXAMPLES_TAG);
}
//=============================================================================
bool
XmlDocExamples::writeHeaderAsHtml(std::string& utf8stream)
{
    utf8stream
        = utf8stream + R"(<link rel="stylesheet" href="mono-blue.css"  type="text/css" />)" + "\n";
    utf8stream = utf8stream + R"(<script src = "highlight.pack.js" type = "text/javascript">)"
        + "\n" + HTML_SCRIPT_OUT_TAG + "\n";
    utf8stream = utf8stream + "<script type = \"text/javascript\">hljs.highlightAll();"
        + HTML_SCRIPT_OUT_TAG + "\n";
    utf8stream = utf8stream + HTML_SCRIPT_OUT_TAG + "\n";
    return true;
}
//=============================================================================
bool
XmlDocExamples::writeAsHtml(std::string& utf8stream)
{
    if (examplesVector.size() > 1) {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Examples") + HTML_H3_OUT_TAG + "\n";
    } else {
        utf8stream = utf8stream + HTML_H3_IN_TAG + _("Example") + HTML_H3_OUT_TAG + "\n";
    }
    utf8stream = utf8stream + HTML_HR_OUT_TAG + "\n";
    utf8stream = utf8stream + "\n";
    for (auto& k : examplesVector) {
        k->writeAsHtml(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
bool
XmlDocExamples::writeHeaderAsMarkdown(std::string& utf8stream)
{
    return true;
}
//=============================================================================
bool
XmlDocExamples::writeAsMarkdown(std::string& utf8stream)
{
    if (examplesVector.size() > 1) {
        utf8stream = utf8stream + "## " + _("Examples") + "\n";
    } else {
        utf8stream = utf8stream + "## " + _("Example") + "\n";
    }
    utf8stream = utf8stream + "\n";
    for (auto& k : examplesVector) {
        k->writeAsMarkdown(utf8stream);
    }
    utf8stream = utf8stream + "\n";
    return true;
}
//=============================================================================
void
XmlDocExamples::setDirectories(const std::wstring& srcDirectory, const std::wstring& dstDirectory)
{
    for (auto& k : examplesVector) {
        k->setDirectories(srcDirectory, dstDirectory);
    }
}
//=============================================================================
}
//=============================================================================
