//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <chrono>
#include <ctime>
#include "Calendar.hpp"
#include "DateVector.hpp"
#include "Transpose.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NBR_DAY_IN_A_WEEK 7
#define NBR_WEEK_IN_A_MONTH_MAX 6
//=============================================================================

// Helper: integer division that floors for negative values
static constexpr long long
div_floor(long long a, long long b) noexcept
{
    long long q = a / b;
    long long r = a % b;
    if ((r != 0) && ((r < 0) != (b < 0)))
        --q;
    return q;
}

// Howard Hinnant algorithms: civil_from_days / days_from_civil
// days = number of days since 1970-01-01 (1970-01-01 -> 0)
static void
civil_from_days(long long z, long long& y, unsigned& m, unsigned& d) noexcept
{
    // z is days since 1970-01-01
    z += 719468; // shift to civil algorithm epoch
    const long long era = div_floor(z, 146097);
    const long long doe = z - era * 146097; // [0, 146096]
    const long long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365; // [0, 399]
    y = static_cast<long long>(yoe) + era * 400;
    const long long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    const long long mp = (5 * doy + 2) / 153; // [0, 11]
    d = static_cast<unsigned>(doy - (153 * mp + 2) / 5 + 1); // [1, 31]
    m = static_cast<unsigned>(mp + (mp < 10 ? 3 : -9)); // [1, 12]
    y += (m <= 2);
}

static long long
days_from_civil(long long y, unsigned m, unsigned d) noexcept
{
    // returns days since 1970-01-01
    long long yy = static_cast<long long>(y) - (m <= 2);
    unsigned mm = m + (m <= 2 ? 12u : 0u);
    long long era = yy / 400;
    long long yoe = yy - era * 400; // [0, 399]
    long long doy = (153 * (mm - 3) + 2) / 5 + d - 1; // [0, 365]
    long long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy; // [0, 146096]
    long long days = era * 146097 + doe - 719468; // shift back to 1970-01-01 epoch
    return days;
}

static bool
is_leap_year(long long y) noexcept
{
    return (y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0));
}

static unsigned
days_in_month(unsigned year_u, unsigned month) noexcept
{
    static const unsigned mdays[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    long long y = static_cast<long long>(year_u);
    if (month == 2) {
        return mdays[1] + (is_leap_year(y) ? 1u : 0u);
    }
    return mdays[month - 1];
}

// Return weekday for a civil date (y,m,d) with 0=Sunday, 1=Monday, ...
static unsigned
weekday_from_ymd(long long y, unsigned m, unsigned d) noexcept
{
    long long days = days_from_civil(y, m, d); // days since 1970-01-01
    // 1970-01-01 was a Thursday. (0 => Sunday)
    long long w = (days + 4) % 7;
    if (w < 0)
        w += 7;
    return static_cast<unsigned>(w);
}

// Get current UTC civil date from system_clock::now() (C++17 has no tz database)
static void
current_utc_ymd(long long& y, unsigned& m, unsigned& d) noexcept
{
    using namespace std::chrono;
    auto now = system_clock::now();
    auto secs = duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    // floor division to days since epoch (UTC)
    long long days = div_floor(secs, 86400);
    civil_from_days(days, y, m, d);
}

//=============================================================================
Calendar::Calendar()
{
    long long yy = 0;
    unsigned mm = 0;
    unsigned dd = 0;
    // Note: we use UTC date because C++17 std::chrono has no built-in timezone database.
    current_utc_ymd(yy, mm, dd);
    y = static_cast<uint64>(yy);
    m = static_cast<uint8>(mm);
}
//=============================================================================
Calendar::Calendar(double dateserial)
{
    double y = 0.;
    double m = 0.;
    double d = 0.;
    double h = 0.;
    double mm = 0.;
    double s = 0.;
    double ms = 0.;
    DateVector(dateserial, y, m, d, h, mm, s, ms, false);
    this->m = static_cast<uint8>(m);
    this->y = static_cast<uint64>(y);
}
//=============================================================================
Calendar::Calendar(uint64 y, uint8 m)
{
    this->y = y;
    this->m = m;
}
//=============================================================================
Calendar::~Calendar()
{
    y = 0;
    m = 0;
}
//=============================================================================
std::wstring
Calendar::getAsFormatedText()
{
    // compute first day weekday and number of days in month using chrono-based helpers
    unsigned firstdate
        = weekday_from_ymd(static_cast<long long>(this->y), static_cast<unsigned>(this->m), 1);
    unsigned dim = days_in_month(static_cast<unsigned>(this->y), static_cast<unsigned>(this->m));
    // fix: last index must be firstdate + dim - 1 (was + dim, off-by-one)
    unsigned lastdate = firstdate + (dim > 0 ? dim - 1 : 0);
    wstringVector names = getNameOfDays();
    std::wstring msg = getMonthName() + L" " + std::to_wstring(getYear()) + L"\n";
    std::wstring msg1 = fmt::sprintf(L"%5s %5s %5s %5s %5s %5s %5s\n", names[0], names[1], names[2],
        names[3], names[4], names[5], names[6]);
    size_t nbBlanks = msg1.size() / 2 - msg.size() / 2;
    msg.insert(0, nbBlanks, L' ');
    msg = msg + msg1;
    double v = 1;
    uint8 k = 0;

    for (unsigned j = 0; j < firstdate; ++j) {
        msg = msg + fmt::sprintf(L"%5d", 0) + L" ";
        if (k == NBR_DAY_IN_A_WEEK - 1) {
            msg = msg + L"\n";
            k = 0;
        } else {
            k++;
        }
    }
    for (unsigned j = firstdate; j <= lastdate; ++j) {
        msg = msg + fmt::sprintf(L"%5d", int(v)) + L" ";
        if (k == NBR_DAY_IN_A_WEEK - 1) {
            msg = msg + L"\n";
            k = 0;
        } else {
            k++;
        }
        v++;
    }
    // trailing blanks to complete 7*6 = 42 cells
    for (unsigned j = lastdate + 1; j < (NBR_DAY_IN_A_WEEK * NBR_WEEK_IN_A_MONTH_MAX); ++j) {
        msg = msg + fmt::sprintf(L"%5d", 0) + L" ";
        if (k == NBR_DAY_IN_A_WEEK - 1) {
            msg = msg + L"\n";
            k = 0;
        } else {
            k++;
        }
    }
    return msg;
}
//=============================================================================
ArrayOf
Calendar::get()
{
    unsigned firstdate
        = weekday_from_ymd(static_cast<long long>(this->y), static_cast<unsigned>(this->m), 1);
    unsigned dim = days_in_month(static_cast<unsigned>(this->y), static_cast<unsigned>(this->m));
    // fix: last index must be firstdate + dim - 1
    unsigned lastdate = firstdate + (dim > 0 ? dim - 1 : 0);

    double* Month = static_cast<double*>(ArrayOf::allocateArrayOf(
        NLS_DOUBLE, NBR_DAY_IN_A_WEEK * NBR_WEEK_IN_A_MONTH_MAX, stringVector(), true));
    double v = 1;
    for (unsigned k = firstdate; k <= lastdate; ++k) {
        Month[k] = v;
        v++;
    }
    Dimensions dimCal(NBR_DAY_IN_A_WEEK, NBR_WEEK_IN_A_MONTH_MAX);
    ArrayOf Cal = ArrayOf(NLS_DOUBLE, dimCal, Month);
    bool needToOverload;
    return Transpose(Cal, needToOverload);
}
//=============================================================================
wstringVector
Calendar::getNameOfDays()
{
    wstringVector names;
    names.reserve(7);
    names.push_back(_W("Sun"));
    names.push_back(_W("M"));
    names.push_back(_W("Tu"));
    names.push_back(_W("W"));
    names.push_back(_W("Th"));
    names.push_back(_W("F"));
    names.push_back(_W("Sat"));
    return names;
}
//=============================================================================
std::wstring
Calendar::getMonthName()
{
    wstringVector month_names;
    month_names.reserve(12);
    month_names.push_back(_W("Jan"));
    month_names.push_back(_W("Feb"));
    month_names.push_back(_W("Mar"));
    month_names.push_back(_W("Apr"));
    month_names.push_back(_W("May"));
    month_names.push_back(_W("Jun"));
    month_names.push_back(_W("Jul"));
    month_names.push_back(_W("Aug"));
    month_names.push_back(_W("Sep"));
    month_names.push_back(_W("Oct"));
    month_names.push_back(_W("Nov"));
    month_names.push_back(_W("Dec"));
    return month_names[this->m - 1];
}
//=============================================================================
uint64
Calendar::getYear()
{
    return this->y;
}
//=============================================================================
uint8
Calendar::getMonth()
{
    return this->m;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
