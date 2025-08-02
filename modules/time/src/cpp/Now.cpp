//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Now.hpp"
#include "DateNumber.hpp"
#include "boost/date_time/posix_time/posix_time.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
double
Now()
{
    boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
    tm pt_tm = to_tm(pt);
    int year = (1900 + pt_tm.tm_year);
    int month = pt_tm.tm_mon + 1;
    int day = pt_tm.tm_mday;
    int hour = pt_tm.tm_hour;
    int minutes = pt_tm.tm_min;
    int secondes = pt_tm.tm_sec;
    return DateNumber(year, month, day, hour, minutes, secondes);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
