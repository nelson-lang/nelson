//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlCorePrivate.hpp"
#include "XmlTransform.hpp"
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdarg>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <libxml/xmlsave.h>
#include <sstream>
//=============================================================================
namespace Nelson::XmlCorePrivate {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    doubleElementLineIndent(const std::string& input)
    {
        std::string output;
        output.reserve(input.size() * 2);
        size_t lineStart = 0;
        while (lineStart < input.size()) {
            size_t lineEnd = input.find('\n', lineStart);
            if (lineEnd == std::string::npos) {
                lineEnd = input.size();
            } else {
                lineEnd++;
            }
            size_t firstNonSpace = lineStart;
            while (firstNonSpace < lineEnd && input[firstNonSpace] == ' ') {
                firstNonSpace++;
            }
            if (firstNonSpace > lineStart && firstNonSpace < lineEnd
                && input[firstNonSpace] == '<') {
                output.append(firstNonSpace - lineStart, ' ');
            }
            output.append(input, lineStart, lineEnd - lineStart);
            lineStart = lineEnd;
        }
        return output;
    }
    //=============================================================================
} // namespace
//=============================================================================
#if LIBXML_VERSION >= 21200
void
xmlStructuredErrorHandler(void* ctx, const xmlError* error)
#else
void
xmlStructuredErrorHandler(void* ctx, xmlError* error)
#endif
{
    if (ctx == nullptr || error == nullptr || error->message == nullptr) {
        return;
    }
    auto* accumulator = static_cast<XmlErrorAccumulator*>(ctx);
    accumulator->message += error->message;
}
//=============================================================================
void
xmlGenericErrorHandler(void* ctx, const char* message, ...)
{
    if (ctx == nullptr || message == nullptr) {
        return;
    }
    char buffer[1024];
    va_list args;
    va_start(args, message);
    int written = std::vsnprintf(buffer, sizeof(buffer), message, args);
    va_end(args);
    if (written <= 0) {
        return;
    }
    auto* accumulator = static_cast<XmlErrorAccumulator*>(ctx);
    accumulator->message.append(
        buffer, static_cast<size_t>(std::min(written, static_cast<int>(sizeof(buffer) - 1))));
}
//=============================================================================
void
beginXmlErrorCapture(XmlErrorAccumulator& errors)
{
    xmlSetGenericErrorFunc(&errors, xmlGenericErrorHandler);
    xmlSetStructuredErrorFunc(&errors, xmlStructuredErrorHandler);
}
//=============================================================================
void
endXmlErrorCapture()
{
    xmlSetStructuredErrorFunc(nullptr, nullptr);
    xmlSetGenericErrorFunc(nullptr, nullptr);
}
//=============================================================================
std::string
trimUtf8(const std::string& value)
{
    size_t first = 0;
    while (first < value.size() && std::isspace(static_cast<unsigned char>(value[first]))) {
        first++;
    }
    size_t last = value.size();
    while (last > first && std::isspace(static_cast<unsigned char>(value[last - 1]))) {
        last--;
    }
    return value.substr(first, last - first);
}
//=============================================================================
bool
writeFileUtf8(const std::wstring& filename, const std::string& data)
{
    std::ofstream file(wstring_to_utf8(filename), std::ios::binary);
    if (!file.is_open()) {
        return false;
    }
    file.write(data.data(), static_cast<std::streamsize>(data.size()));
    return file.good();
}
//=============================================================================
bool
readFileUtf8(const std::wstring& filename, std::string& data)
{
    std::ifstream file(wstring_to_utf8(filename), std::ios::binary);
    if (!file.is_open()) {
        return false;
    }
    std::ostringstream buffer;
    buffer << file.rdbuf();
    data = buffer.str();
    return true;
}
//=============================================================================
std::wstring
temporaryXmlFilename()
{
    auto ticks = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    std::filesystem::path path = std::filesystem::temp_directory_path()
        / ("nelson_xml_" + std::to_string(static_cast<long long>(ticks)) + ".xml");
    return path.wstring();
}
//=============================================================================
xmlDocPtr
parseXmlFile(const std::wstring& filename, bool allowDoctype, std::wstring& errorMessage)
{
    XmlErrorAccumulator errors;
    beginXmlErrorCapture(errors);
    xmlDocPtr doc = xmlReadFile(wstring_to_utf8(filename).c_str(), nullptr, XML_PARSE_NONET);
    endXmlErrorCapture();
    if (doc == nullptr) {
        errorMessage = errors.message.empty() ? L"Unable to parse XML file."
                                              : utf8_to_wstring(errors.message);
        return nullptr;
    }
    if (!allowDoctype && doc->intSubset != nullptr) {
        xmlFreeDoc(doc);
        errorMessage = L"DOCTYPE declarations are not permitted.";
        return nullptr;
    }
    return doc;
}
//=============================================================================
xmlDocPtr
parseXmlMemory(const std::string& data, const char* url, const char* encoding, int options,
    std::wstring& errorMessage, const std::wstring& fallbackMessage)
{
    XmlErrorAccumulator errors;
    beginXmlErrorCapture(errors);
    xmlDocPtr doc
        = xmlReadMemory(data.c_str(), static_cast<int>(data.size()), url, encoding, options);
    endXmlErrorCapture();
    if (doc == nullptr) {
        errorMessage = fallbackMessage.empty() && !errors.message.empty()
            ? utf8_to_wstring(errors.message)
            : fallbackMessage;
    }
    return doc;
}
//=============================================================================
bool
serializeDoc(xmlDocPtr doc, bool prettyPrint, std::string& text, std::wstring& errorMessage)
{
    xmlBufferPtr buffer = xmlBufferCreate();
    if (buffer == nullptr) {
        errorMessage = L"Unable to serialize XML document.";
        return false;
    }
    int options = XML_SAVE_NO_EMPTY;
    if (prettyPrint) {
        options |= XML_SAVE_FORMAT;
    }
    xmlSaveCtxtPtr context = xmlSaveToBuffer(buffer, "UTF-8", options);
    if (context == nullptr) {
        xmlBufferFree(buffer);
        errorMessage = L"Unable to serialize XML document.";
        return false;
    }
    int result = xmlSaveDoc(context, doc);
    int closeResult = xmlSaveClose(context);
    if (result < 0 || closeResult < 0) {
        xmlBufferFree(buffer);
        errorMessage = L"Unable to serialize XML document.";
        return false;
    }
    text.assign(reinterpret_cast<const char*>(xmlBufferContent(buffer)),
        static_cast<size_t>(xmlBufferLength(buffer)));
    xmlBufferFree(buffer);
    if (prettyPrint) {
        text = doubleElementLineIndent(text);
    }
    return true;
}
//=============================================================================
ArrayOf
makeXmlDocumentObject(const std::string& text)
{
    stringVector names = { "Text" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(text);
    return ArrayOf::classdefArray("xmlDocument", names, values);
}
//=============================================================================
bool
isXmlDocumentObject(const ArrayOf& value)
{
    return value.isClassType() && value.isScalar() && value.classType() == "xmlDocument"
        && value.isField("Text");
}
//=============================================================================
} // namespace Nelson::XmlCorePrivate
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
XmlReadDocument(
    const std::wstring& filename, const XmlReadOptions& options, std::wstring& errorMessage)
{
    xmlDocPtr doc = XmlCorePrivate::parseXmlFile(filename, options.allowDoctype, errorMessage);
    if (doc == nullptr) {
        return ArrayOf::empty();
    }
    std::string text;
    bool ok = XmlCorePrivate::serializeDoc(doc, true, text, errorMessage);
    xmlFreeDoc(doc);
    return ok ? XmlCorePrivate::makeXmlDocumentObject(text) : ArrayOf::empty();
}
//=============================================================================
bool
XmlDocumentToText(const ArrayOf& domNode, std::wstring& text, std::wstring& errorMessage)
{
    if (XmlCorePrivate::isXmlDocumentObject(domNode)) {
        text = domNode.field("Text").asWideString();
        return true;
    }
    if (domNode.isCharacterArray() || domNode.isScalarStringArray()) {
        text = domNode.asWideString();
        return true;
    }
    errorMessage = L"XML DOM node expected.";
    return false;
}
//=============================================================================
bool
XmlWriteDocument(const std::wstring& filename, const ArrayOf& domNode, std::wstring& errorMessage)
{
    std::wstring text;
    if (!XmlDocumentToText(domNode, text, errorMessage)) {
        return false;
    }
    std::string data = wstring_to_utf8(text);
    xmlDocPtr doc = XmlCorePrivate::parseXmlMemory(
        data, nullptr, "UTF-8", XML_PARSE_NONET, errorMessage, L"Unable to parse XML DOM node.");
    if (doc == nullptr) {
        return false;
    }
    std::string output;
    bool ok = XmlCorePrivate::serializeDoc(doc, true, output, errorMessage);
    xmlFreeDoc(doc);
    return ok && XmlCorePrivate::writeFileUtf8(filename, output);
}
//=============================================================================
bool
XmlTransformToText(const std::wstring& xmlfile, const std::wstring& xslfile, std::wstring& output,
    std::wstring& errorMessage)
{
    std::wstring tempFile = XmlCorePrivate::temporaryXmlFilename();
    if (!XmlTransform(xmlfile, xslfile, tempFile, true, DOCUMENT_OUTPUT::UNDEFINED, errorMessage)) {
        return false;
    }
    std::string data;
    bool ok = XmlCorePrivate::readFileUtf8(tempFile, data);
    std::error_code code;
    std::filesystem::remove(std::filesystem::path(tempFile), code);
    if (!ok) {
        errorMessage = L"Unable to read transformed XML text.";
        return false;
    }
    output = utf8_to_wstring(data);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
