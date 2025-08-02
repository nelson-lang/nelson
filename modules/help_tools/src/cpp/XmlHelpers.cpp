//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include "XmlHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
std::wstring
getXmlFileEncoding(const std::wstring& filename)
{
    std::wstring encoding = L"UTF-8";
#ifdef _MSC_VER
    std::ifstream xmlDocFile(filename);
#else
    std::ifstream xmlDocFile(wstring_to_utf8(filename));
#endif
    std::string xmlDocString = "";
    if (xmlDocFile.is_open()) {
        std::string tmpline;
        while (safegetline(xmlDocFile, tmpline)) {
            xmlDocString += tmpline + '\n';
        }
        xmlDocFile.close();
    }
    xmlDocPtr doc = xmlParseMemory(xmlDocString.c_str(), (int)xmlDocString.size());
    if (doc) {
        if (doc->encoding) {
            encoding = utf8_to_wstring((char*)(doc->encoding));
        }
    }
    xmlFreeDoc(doc);
    return encoding;
}
//=============================================================================
static std::wstring* errorBuffer = nullptr;
//=============================================================================
static void
errorFunctionWithoutOutput(void* ctx, const char* msg, ...)
{
}
//=============================================================================
void
errorFunction(void* ctx, const char* msg, ...)
{
    char str[4096];
    va_list args;
    va_start(args, msg);
    vsnprintf(str, 4096, msg, args);
    va_end(args);
    errorBuffer->append(utf8_to_wstring(str));
}
//=============================================================================
xmlParserCtxt*
initContext(std::wstring& error, bool validate)
{
    xmlParserCtxt* ctxt;
    ctxt = xmlNewParserCtxt();
    if (!ctxt) {
        error = _W("Cannot create a parser context");
        return nullptr;
    }
    if (errorBuffer) {
        delete errorBuffer;
    }
    errorBuffer = new std::wstring();
    if (validate) {
        ctxt->vctxt.error = (xmlValidityErrorFunc)errorFunction;
    }
    xmlSetGenericErrorFunc(ctxt, errorFunction);
    return ctxt;
}
//=============================================================================
xmlDoc*
readDocument(const std::string& xmlCode, bool validate, std::wstring& error)
{
    xmlParserCtxt* ctxt = initContext(error, validate);
    xmlDoc* doc = nullptr;
    int options = XML_PARSE_NSCLEAN | XML_PARSE_NOBLANKS;
    if (validate) {
        options = options | XML_PARSE_DTDVALID;
    }
    if (!ctxt) {
        xmlSetGenericErrorFunc(nullptr, errorFunctionWithoutOutput);
        return nullptr;
    }
    doc = xmlCtxtReadDoc(ctxt, (const xmlChar*)xmlCode.c_str(), nullptr, nullptr, options);
    if (!doc || !ctxt->valid) {
        error = *errorBuffer;
    }
    xmlSetGenericErrorFunc(nullptr, errorFunctionWithoutOutput);
    xmlFreeParserCtxt(ctxt);
    return doc;
}
//=============================================================================

}
//=============================================================================
