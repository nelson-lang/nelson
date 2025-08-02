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
#include "nlsSpreadsheet_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSPREADSHEET_IMPEXP writeTableOptions
{
public:
    // common
    bool _WriteRowNames = false;
    std::string _FileType = "";
    // csv
    bool _WriteVariableNames = false;
    std::string _WriteMode = "";
    char _Delimiter = '\0';
    std::string _QuoteStrings = "";
    // xml
    std::string _RowNodeName = "";
    std::string _TableNodeName = "";
    std::string _AttributeSuffix = "";
};
//=============================================================================
NLSSPREADSHEET_IMPEXP void
WriteTable(const ArrayOf& table, const std::wstring& filename, const writeTableOptions& options,
    std::wstring& errorMessage);
//=============================================================================
};
//=============================================================================
