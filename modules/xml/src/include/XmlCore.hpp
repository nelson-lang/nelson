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
#include "ArrayOf.hpp"
#include "nlsXml_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct NLSXML_IMPEXP XmlReadOptions
{
    bool allowDoctype = true;
    std::string xmlEngine = "auto";
};
//=============================================================================
struct NLSXML_IMPEXP XmlStructReadOptions
{
    std::string fileType = "";
    std::wstring structNodeName = L"";
    std::wstring structSelector = L"";
    bool importAttributes = true;
    std::string attributeSuffix = "Attribute";
    std::vector<std::pair<std::string, std::string>> registeredNamespaces;
};
//=============================================================================
struct NLSXML_IMPEXP XmlStructWriteOptions
{
    std::string fileType = "";
    std::string structNodeName = "struct";
    std::string attributeSuffix = "Attribute";
    bool prettyPrint = true;
};
//=============================================================================
NLSXML_IMPEXP ArrayOf
XmlReadDocument(
    const std::wstring& filename, const XmlReadOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP bool
XmlWriteDocument(const std::wstring& filename, const ArrayOf& domNode, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP bool
XmlDocumentToText(const ArrayOf& domNode, std::wstring& text, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP ArrayOfVector
XmlDomMethod(const std::string& method, const ArrayOfVector& args, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP ArrayOf
XmlReadStruct(
    const std::wstring& filename, const XmlStructReadOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP bool
XmlWriteStruct(const ArrayOf& value, const std::wstring& filename,
    const XmlStructWriteOptions& options, std::wstring& errorMessage);
//=============================================================================
NLSXML_IMPEXP bool
XmlTransformToText(const std::wstring& xmlfile, const std::wstring& xslfile, std::wstring& output,
    std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
