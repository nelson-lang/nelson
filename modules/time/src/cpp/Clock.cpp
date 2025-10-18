//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Clock.hpp"
#include <chrono>
#include <ctime>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Clock()
{
    std::time_t now = std::time(nullptr);
    std::tm pt_tm {};
#ifdef _MSC_VER
    localtime_s(&pt_tm, &now);
#else
    localtime_r(&now, &pt_tm);
#endif
    double* vect
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 6, stringVector(), false));
    vect[0] = 1900 + pt_tm.tm_year;
    vect[1] = pt_tm.tm_mon + 1;
    vect[2] = pt_tm.tm_mday;
    vect[3] = pt_tm.tm_hour;
    vect[4] = pt_tm.tm_min;
    vect[5] = pt_tm.tm_sec;
    Dimensions dim(1, 6);
    return ArrayOf(NLS_DOUBLE, dim, (void*)vect);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
