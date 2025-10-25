//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlerror.h>
#include <libxml/xmlschemastypes.h>
#include <fstream>
#include <sstream>
#include "XmlXsdChecker.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct XmlErrorAccumulator
{
    wstringVector errors;
    wstringVector warnings;
};
//=============================================================================
#if LIBXML_VERSION >= 21200
static void
xmlStructuredErrorHandler(void* ctx, const xmlError* error)
#else
static void
xmlStructuredErrorHandler(void* ctx, xmlError* error)
#endif
{
    XmlErrorAccumulator* acc = reinterpret_cast<XmlErrorAccumulator*>(ctx);
    if (!acc || !error)
        return;
    std::wstring msg = utf8_to_wstring(error->message ? error->message : "");
    msg += L" [line " + std::to_wstring(error->line) + L", col " + std::to_wstring(error->int2)
        + L"]";
    if (error->level == XML_ERR_WARNING) {
        acc->warnings.push_back(msg);
    } else {
        acc->errors.push_back(msg);
    }
}
//=============================================================================
bool
xmlDocXsdChecker(const std::wstring& xmlFilename, const std::wstring& xsdFilename,
    wstringVector& errorMessage, wstringVector& warningMessage)
{
    errorMessage.clear();
    warningMessage.clear();
    XmlErrorAccumulator errorAccumulator;
    xmlSetStructuredErrorFunc(&errorAccumulator, xmlStructuredErrorHandler);

    // Convert filenames to UTF-8
    std::string xmlFileUtf8 = wstring_to_utf8(xmlFilename);
    std::string xsdFileUtf8 = wstring_to_utf8(xsdFilename);

    // Read XML file into string

#ifdef _MSC_VER
    std::ifstream xmlFileStream(xmlFilename, std::ios::binary);
#else
    std::ifstream xmlFileStream(xmlFileUtf8, std::ios::binary);
#endif
    if (!xmlFileStream) {
        errorMessage.push_back(_W("Failed to open XML file: ") + xmlFilename);
        xmlSetStructuredErrorFunc(NULL, NULL);
        return false;
    }
    std::ostringstream buffer;
    buffer << xmlFileStream.rdbuf();
    std::string xmlContent = buffer.str();

    // Parse the XML document from memory
    xmlDocPtr doc = xmlReadMemory(
        xmlContent.c_str(), static_cast<int>(xmlContent.size()), xmlFileUtf8.c_str(), NULL, 0);
    if (!doc) {
        errorMessage.push_back(_W("Failed to parse XML file: ") + xmlFilename);
        for (const auto& msg : errorAccumulator.errors)
            errorMessage.push_back(msg);
        xmlSetStructuredErrorFunc(NULL, NULL);
        return false;
    }

    // Load the XSD schema
    xmlSchemaParserCtxtPtr parserCtxt = xmlSchemaNewParserCtxt(xsdFileUtf8.c_str());
    if (!parserCtxt) {
        errorMessage.push_back(_W("Failed to create XSD schema parser context: ") + xsdFilename);
        xmlFreeDoc(doc);
        xmlSetStructuredErrorFunc(NULL, NULL);
        return false;
    }

    xmlSchemaPtr schema = xmlSchemaParse(parserCtxt);
    if (!schema) {
        errorMessage.push_back(_W("Failed to parse XSD schema: ") + xsdFilename);
        xmlSchemaFreeParserCtxt(parserCtxt);
        xmlFreeDoc(doc);
        xmlSetStructuredErrorFunc(NULL, NULL);
        return false;
    }

    xmlSchemaValidCtxtPtr validCtxt = xmlSchemaNewValidCtxt(schema);
    if (!validCtxt) {
        errorMessage.push_back(_W("Failed to create XSD schema validation context."));
        xmlSchemaFree(schema);
        xmlSchemaFreeParserCtxt(parserCtxt);
        xmlFreeDoc(doc);
        xmlSetStructuredErrorFunc(NULL, NULL);
        return false;
    }

    // Validate the XML document against the schema
    bool valid = (xmlSchemaValidateDoc(validCtxt, doc) == 0);

    // Gather errors and warnings
    for (const auto& msg : errorAccumulator.errors) {
        errorMessage.push_back(msg);
    }
    for (const auto& msg : errorAccumulator.warnings) {
        warningMessage.push_back(msg);
    }

    xmlSchemaFreeValidCtxt(validCtxt);
    xmlSchemaFree(schema);
    xmlSchemaFreeParserCtxt(parserCtxt);
    xmlFreeDoc(doc);
    xmlSetStructuredErrorFunc(NULL, NULL);

    return valid;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
