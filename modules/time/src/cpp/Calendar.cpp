//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <boost/date_time/gregorian/greg_date.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
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
Calendar::Calendar()
{
    boost::posix_time::ptime pt = boost::posix_time::microsec_clock::local_time();
    tm pt_tm = to_tm(pt);
    y = (1900 + pt_tm.tm_year);
    m = pt_tm.tm_mon + 1;
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
    boost::gregorian::date d1 { (boost::gregorian::gregorian_calendar::year_type)(unsigned short)y,
        (boost::gregorian::gregorian_calendar::month_type)m, 1 };
    boost::gregorian::date d2 { d1.end_of_month() };
    boost::gregorian::date_period month_period(d1, d2);
    wstringVector names = getNameOfDays();
    std::wstring msg = getMonthName() + L" " + std::to_wstring(getYear()) + L"\n";
    std::wstring msg1 = fmt::sprintf(L"%5s %5s %5s %5s %5s %5s %5s\n", names[0], names[1], names[2],
        names[3], names[4], names[5], names[6]);
    size_t nbBlanks = msg1.size() / 2 - msg.size() / 2;
    msg.insert(0, nbBlanks, L' ');
    msg = msg + msg1;
    double v = 1;
    uint8 firstdate = (uint8)(d1.day_of_week().as_number());
    uint8 lastdate = (uint8)(month_period.length().days() + firstdate);
    uint8 k = 0;

    for (uint8 j = 0; j < firstdate; j++) {
        msg = msg + fmt::sprintf(L"%5d", 0) + L" ";
        if (k == NBR_DAY_IN_A_WEEK - 1) {
            msg = msg + L"\n";
            k = 0;
        } else {
            k++;
        }
    }
    for (uint8 j = firstdate; j <= lastdate; j++) {
        msg = msg + fmt::sprintf(L"%5d", int(v)) + L" ";
        if (k == NBR_DAY_IN_A_WEEK - 1) {
            msg = msg + L"\n";
            k = 0;
        } else {
            k++;
        }
        v++;
    }
    for (uint8 j = lastdate + 1; j < (NBR_DAY_IN_A_WEEK * (NBR_DAY_IN_A_WEEK - 1)); j++) {
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
    boost::gregorian::date d1 { (boost::gregorian::gregorian_calendar::year_type)(unsigned short)y,
        (boost::gregorian::gregorian_calendar::month_type)m, 1 };
    boost::gregorian::date d2 { d1.end_of_month() };
    boost::gregorian::date_period month_period(d1, d2);
    double* Month = static_cast<double*>(ArrayOf::allocateArrayOf(
        NLS_DOUBLE, NBR_DAY_IN_A_WEEK * NBR_WEEK_IN_A_MONTH_MAX, stringVector(), true));
    double v = 1;
    uint8 firstdate = (uint8)(d1.day_of_week().as_number());
    uint8 lastdate = (uint8)(month_period.length().days() + firstdate);
    for (uint8 k = firstdate; k <= lastdate; k++) {
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
