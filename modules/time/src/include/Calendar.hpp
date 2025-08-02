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
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
class NLSTIME_IMPEXP Calendar
{
public:
    Calendar();
    Calendar(double dateserial);
    Calendar(uint64 y, uint8 m);
    ~Calendar();
    std::wstring
    getAsFormatedText();
    ArrayOf
    get();
    wstringVector
    getNameOfDays();
    std::wstring
    getMonthName();
    uint64
    getYear();
    uint8
    getMonth();

private:
    uint64 y = 0;
    uint8 m = 0;
};
} // namespace Nelson
//=============================================================================
