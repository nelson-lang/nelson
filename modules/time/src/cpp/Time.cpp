//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#endif
//=============================================================================
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/date_time/posix_time/ptime.hpp>
#include <boost/chrono/chrono.hpp>
#include "Time.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
uint64
TimeAsNanoSeconds()
{
    boost::posix_time::ptime now(boost::posix_time::microsec_clock::universal_time());
    boost::posix_time::ptime const EPOCH(boost::gregorian::date(1970, 1, 1));
    boost::posix_time::time_duration delta(now - EPOCH);
    return uint64(static_cast<boost::uint64_t>(delta.total_nanoseconds()));
}
//=============================================================================
double
TimeAsSeconds()
{
    uint64 _now = TimeAsNanoSeconds();
    return double(_now) * 1e-9;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
