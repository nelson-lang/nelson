//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "XmlCore.hpp"
#include "characters_encoding.hpp"
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <string>
//=============================================================================
namespace Nelson::XmlCorePrivate {
//=============================================================================
struct XmlErrorAccumulator
{
    std::string message;
};
//=============================================================================
#if LIBXML_VERSION >= 21200
void
xmlStructuredErrorHandler(void* ctx, const xmlError* error);
#else
void
xmlStructuredErrorHandler(void* ctx, xmlError* error);
#endif
//=============================================================================
void
xmlGenericErrorHandler(void* ctx, const char* message, ...);
//=============================================================================
void
beginXmlErrorCapture(XmlErrorAccumulator& errors);
//=============================================================================
void
endXmlErrorCapture();
//=============================================================================
std::string
trimUtf8(const std::string& value);
//=============================================================================
bool
writeFileUtf8(const std::wstring& filename, const std::string& data);
//=============================================================================
bool
readFileUtf8(const std::wstring& filename, std::string& data);
//=============================================================================
std::wstring
temporaryXmlFilename();
//=============================================================================
xmlDocPtr
parseXmlFile(const std::wstring& filename, bool allowDoctype, std::wstring& errorMessage);
//=============================================================================
xmlDocPtr
parseXmlMemory(const std::string& data, const char* url, const char* encoding, int options,
    std::wstring& errorMessage, const std::wstring& fallbackMessage);
//=============================================================================
bool
serializeDoc(xmlDocPtr doc, bool prettyPrint, std::string& text, std::wstring& errorMessage);
//=============================================================================
ArrayOf
makeXmlDocumentObject(const std::string& text);
//=============================================================================
bool
isXmlDocumentObject(const ArrayOf& value);
//=============================================================================
} // namespace Nelson::XmlCorePrivate
//=============================================================================
