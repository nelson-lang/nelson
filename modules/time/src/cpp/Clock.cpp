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
#include "boost/date_time/posix_time/posix_time.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Clock()
{
    boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
    tm pt_tm = to_tm(pt);
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
