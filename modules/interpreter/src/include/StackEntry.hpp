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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP StackEntry
{
public:
    std::string cname;
    std::string detail;
    int tokid { 0 };

    StackEntry();
    StackEntry(const std::string& cntxt, const std::string& detail, int id);
    ~StackEntry();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
