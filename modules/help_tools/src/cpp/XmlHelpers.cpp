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
#include "XmlHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <fstream>
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
getXmlFileEncoding(std::wstring filename)
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
{}
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
        return 0;
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
    xmlDoc* doc = 0;
    int options = XML_PARSE_NSCLEAN | XML_PARSE_NOBLANKS;
    if (validate) {
        options = options | XML_PARSE_DTDVALID;
    }
    if (!ctxt) {
        xmlSetGenericErrorFunc(0, errorFunctionWithoutOutput);
        return 0;
    }
    doc = xmlCtxtReadDoc(ctxt, (const xmlChar*)xmlCode.c_str(), 0, 0, options);
    if (!doc || !ctxt->valid) {
        error = *errorBuffer;
    }
    xmlSetGenericErrorFunc(0, errorFunctionWithoutOutput);
    xmlFreeParserCtxt(ctxt);
    return doc;
}
//=============================================================================

}
//=============================================================================
