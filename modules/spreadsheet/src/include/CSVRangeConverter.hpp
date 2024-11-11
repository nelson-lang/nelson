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
#include <string>
#include <vector>
#include "nlsSpreadsheet_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSPREADSHEET_IMPEXP CSVRangeConverter
{
    //=============================================================================
private:
    static double
    columnToNumber(const std::string& column, bool& failed);
    static double
    extractRow(const std::string& cell, bool& failed);
    static std::string
    extractColumn(const std::string& cell, bool& failed);
    //=============================================================================
public:
    static std::vector<double>
    convertRange(const std::string& range, bool& failed);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
