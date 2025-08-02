//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
