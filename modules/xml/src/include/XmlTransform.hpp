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
#include "nlsXml_exports.h"
#include "XmlTarget.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSXML_IMPEXP bool
XmlTransform(const std::wstring& xmlfile, const std::wstring& xslfile,
    const std::wstring& outputfile, bool overwrite, DOCUMENT_OUTPUT documentOutput,
    std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
