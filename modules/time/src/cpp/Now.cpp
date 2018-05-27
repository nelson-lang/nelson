//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
}
//=============================================================================
