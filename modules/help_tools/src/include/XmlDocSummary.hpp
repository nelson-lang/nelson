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
#include "nlsHelp_tools_exports.h"
#include "XmlTarget.hpp"
#include "XmlDocListOfFiles.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
XmlDocTocSummary(const std::wstring& destinationDirectory, std::vector<XMLDOCFILES>& xmlDocFiles,
    const std::wstring& xsltTocFilename, const std::wstring& xsltSummaryFilename,
    DOCUMENT_OUTPUT outputDocumentType, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
