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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "DateNumber.hpp"
#include "IEEEFP.hpp"
#include "IsLeapYear.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "Types.hpp"
#include <algorithm>
#include <map>
#include <memory>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <regex>
#include <vector>
#include <string>
#include <functional>
//=============================================================================
namespace Nelson {
//=============================================================================
double
DateNumber(double year, double month, double day, double hour, double minutes, double seconds,
    double milliseconds)
{
    if (!IsFinite(year) || !IsFinite(month) || !IsFinite(day) || !IsFinite(hour)
        || !IsFinite(minutes) || !IsFinite(seconds) || !IsFinite(milliseconds)) {
        return nan("");
    }
    if (month < 1) {
        month = 1;
    }
    if (month > 12) {
        year += std::floor((month - 1) / 12.0);
        month = std::fmod(month - 1, 12.0) + 1;
    }

    double decimal_part = 0.;
    if (milliseconds == 0.) {
        decimal_part = (seconds * (1.0 / (24.0 * 3600.0))) + (minutes * (1.0 / (24.0 * 60.0)))
            + (hour * (1.0 / 24.0));
    } else {
        decimal_part = (milliseconds * (1.0 / (24.0 * 3600.0 * 1000.0)))
            + (seconds * (1.0 / (24.0 * 3600.0))) + (minutes * (1.0 / (24.0 * 60.0)))
            + (hour * (1.0 / 24.0));
    }

    int integer_part = static_cast<int>(day + floor((month * 3057 - 3007) / 100));
    // subtract 1 when the month is after February
    integer_part = integer_part + (static_cast<int>(month < 3) - 1);
    // subtract 1 more when the month is after February and the year is not a leap year
    integer_part = (integer_part
        + ((static_cast<int>(month < 3) | static_cast<int>(IsLeapYear(static_cast<int>(year))))
            - 1));
    if (IsLeapYear(static_cast<int>(year))) {
        double leap_year_case = year * 365 + (year / 4) - floor(year / 100) + floor(year / 400);
        integer_part = static_cast<int>(integer_part + leap_year_case);
    } else {
        double not_leap_year_case
            = year * 365 + floor(year / 4) + 1 - floor(year / 100) + floor(year / 400);
        integer_part = static_cast<int>(integer_part + not_leap_year_case);
    }
    return (integer_part + decimal_part);
}
//=============================================================================
static int
monthStringToNumber(const std::wstring& month)
{
    static std::map<std::wstring, int> monthMap = { { L"jan", 1 }, { L"feb", 2 }, { L"mar", 3 },
        { L"apr", 4 }, { L"may", 5 }, { L"jun", 6 }, { L"jul", 7 }, { L"aug", 8 }, { L"sep", 9 },
        { L"oct", 10 }, { L"nov", 11 }, { L"dec", 12 } };
    std::wstring lowerMonth = month;
    std::transform(lowerMonth.begin(), lowerMonth.end(), lowerMonth.begin(), ::tolower);
    auto found = monthMap.find(lowerMonth);
    // 0 means "not a valid month name": callers must reject the date.
    return (found != monthMap.end()) ? found->second : 0;
}
//=============================================================================
static int
parseYearWithPivot(int year, int pivotYear)
{
    if (year < 100) {
        int century = (pivotYear / 100) * 100;
        int pivot = pivotYear % 100;
        if (year <= pivot) {
            year += century;
        } else {
            year += century - 100;
        }
    }
    return year;
}
//=============================================================================
static bool
isValidDate(int year, int month, int day)
{
    if (month < 1 || month > 12) {
        return false;
    }
    if (day < 1 || day > 31) {
        return false;
    }
    if ((month == 4 || month == 6 || month == 9 || month == 11) && day > 30) {
        return false;
    }
    if (month == 2) {
        bool isLeapYear = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
        if (day > (isLeapYear ? 29 : 28)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
std::vector<std::wstring>
splitString(const std::wstring& str, const std::wstring& delims)
{
    std::vector<std::wstring> tokens;
    std::wstring::size_type beg = 0;
    for (auto end = str.find_first_of(delims); end != std::wstring::npos;
        end = str.find_first_of(delims, beg)) {
        if (end > beg) {
            tokens.push_back(str.substr(beg, end - beg));
        }
        beg = end + 1;
    }
    if (beg < str.length()) {
        tokens.push_back(str.substr(beg));
    }
    return tokens;
}
//=============================================================================
struct DateFormatInfo
{
    std::wregex regex;
    std::wstring format;
    std::function<double(const std::wsmatch&, int)> parser;
};
//=============================================================================
static std::vector<DateFormatInfo>
getDateFormatInfoList(bool withPivot)
{
    enum class DateFormat
    {
        DMY,
        MDY
    };

#ifdef _MSC_VER
    __time64_t rawtime;
    _time64(&rawtime);
    auto now = _localtime64(&rawtime);
#else
    time_t rawtime;
    time(&rawtime);
    struct tm t_result;
    auto now = localtime_r(&rawtime, &t_result);
#endif

    int currentYear = now->tm_year + 1900;
    int century = (currentYear / 100) * 100;

    std::vector<DateFormatInfo> formats = {
        { std::wregex(L"(\\d{1,2})[-\\s](\\w{3})[-\\s](\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"dd-mmm-yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                int month = monthStringToNumber(m[2]);
                int day = std::stoi(m[1]);
                if (month == 0 || !isValidDate(year, month, day)) {
                    return std::nan("");
                }
                return DateNumber(
                    year, month, day, std::stoi(m[4]), std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\d{1,2})[-\\s](\\w{3})[-\\s](\\d{4})\\s(\\d{1,2}):(\\d{2})"),
            L"dd-mmm-yyyy HH:MM",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                int month = monthStringToNumber(m[2]);
                int day = std::stoi(m[1]);
                if (month == 0 || !isValidDate(year, month, day)) {
                    return std::nan("");
                }
                return DateNumber(year, month, day, std::stoi(m[4]), std::stoi(m[5]), 0);
            } },
        { std::wregex(L"(\\d{1,2})[-\\s](\\w{3})[-\\s](\\d{4})"), L"dd-mmm-yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                int month = monthStringToNumber(m[2]);
                int day = std::stoi(m[1]);
                if (month == 0 || !isValidDate(year, month, day)) {
                    return std::nan("");
                }
                return DateNumber(year, month, day, 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})/(\\d{1,2})/(\\d{2})"), L"mm/dd/yy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                int year = withPivot ? std::stoi(m[3]) + pivotYear : century + std::stoi(m[3]);
                int first = std::stoi(m[2]);
                int second = std::stoi(m[1]);
                if (isValidDate(year, second, first)) {
                    return DateNumber(year, second, first, 0, 0, 0);
                } else if (isValidDate(year, first, second)) {
                    return DateNumber(year, first, second, 0, 0, 0);
                }
                return std::nan("");
            } },
        { std::wregex(L"(\\d{1,2})-(\\d{1,2})-(\\d{2})"), L"dd-mm-yy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                int day = std::stoi(m[1]);
                int month = std::stoi(m[2]);
                int year = std::stoi(m[3]);
                if (withPivot) {
                    year = pivotYear + year;
                } else {
                    year = century + year;
                }
                return DateNumber(year, month, day, 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})/(\\d{1,2})"), L"mm/dd",
            [withPivot, currentYear](const std::wsmatch& m, int pivotYear) -> double {
                int year = withPivot ? parseYearWithPivot(currentYear, pivotYear) : currentYear;
                return DateNumber(year, std::stoi(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2}):(\\d{2}):(\\d{2})(?:\\s(AM|PM))?", std::regex_constants::icase),
            L"HH:MM:SS PM",
            [withPivot, currentYear](const std::wsmatch& m, int pivotYear) -> double {
                if (!m[4].matched) {
                    return std::nan("");
                }
                int hour = std::stoi(m[1]);
                int days = 1;
                if (m[4] == L"PM" || m[4] == L"pm") {
                    if (hour > 12) {
                        hour -= 12;
                        days += 1;
                    } else {
                        hour += 12;
                    }
                }
                if ((m[4] == L"AM" || m[4] == L"am") && hour == 12) {
                    hour = 0;
                }
                int year = withPivot ? parseYearWithPivot(currentYear, pivotYear) : currentYear;
                return DateNumber(year, 1, days, hour, std::stoi(m[2]), std::stoi(m[3]));
            } },

        { std::wregex(L"(\\d{1,2}):(\\d{2})(?:\\s(AM|PM))?", std::regex_constants::icase),
            L"HH:MM PM",
            [withPivot, currentYear](const std::wsmatch& m, int pivotYear) -> double {
                if (!m[3].matched) {
                    return std::nan("");
                }
                int hour = std::stoi(m[1]);
                int days = 1;
                if (m[3] == L"PM" || m[3] == L"pm") {
                    if (hour > 12) {
                        hour -= 12;
                        days += 1;
                    } else {
                        hour += 12;
                    }
                }
                if ((m[3] == L"AM" || m[3] == L"am") && hour == 12) {
                    hour = 0;
                }
                int year = withPivot ? parseYearWithPivot(currentYear, pivotYear) : currentYear;
                return DateNumber(year, 1, days, hour, std::stoi(m[2]), 0);
            } },
        { std::wregex(L"(\\d{1,2})\\/(\\d{1,2})\\/(\\d{4})"), L"mm/dd/yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                int first = std::stoi(m[1]);
                int second = std::stoi(m[2]);
                if (isValidDate(year, first, second)) {
                    return DateNumber(year, first, second, 0, 0, 0);
                } else if (isValidDate(year, second, first)) {
                    return DateNumber(year, second, first, 0, 0, 0);
                }
                return std::nan("");
            } },
        { std::wregex(L"(\\d{1,2}):(\\d{2}):(\\d{2})?", std::regex_constants::icase), L"HH:MM:SS",
            [withPivot, currentYear](const std::wsmatch& m, int pivotYear) -> double {
                int hour = std::stoi(m[1]);
                // seconds group is optional: '12:34:' is parsed as '12:34'
                int seconds = m[3].matched ? std::stoi(m[3]) : 0;
                int year = withPivot ? parseYearWithPivot(currentYear, pivotYear) : currentYear;
                return DateNumber(year, 1, 1, hour, std::stoi(m[2]), seconds);
            } },
        { std::wregex(L"(\\d{1,2}):(\\d{2})?", std::regex_constants::icase), L"HH:MM",
            [withPivot, currentYear](const std::wsmatch& m, int pivotYear) -> double {
                if (!m[2].matched) {
                    return std::nan("");
                }
                int hour = std::stoi(m[1]);
                int year = withPivot ? parseYearWithPivot(currentYear, pivotYear) : currentYear;
                return DateNumber(year, 1, 1, hour, std::stoi(m[2]), 0);
            } },
        { std::wregex(L"(\\d{1,2})/(\\d{1,2})/(\\d{2})"), L"mm/dd/yy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                int first = std::stoi(m[1]);
                int second = std::stoi(m[2]);
                int year = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear)
                                     : century + std::stoi(m[3]);
                if (isValidDate(year, second, first)) {
                    return DateNumber(year, second, first, 0, 0, 0);
                } else if (isValidDate(year, first, second)) {
                    return DateNumber(year, first, second, 0, 0, 0);
                }
                return std::nan("");
            } },
        { std::wregex(L"(\\d{1,2})/(\\d{1,2})/(\\d{2})"), L"dd/mm/yy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                int year = withPivot ? std::stoi(m[3]) + pivotYear : century + std::stoi(m[3]);
                int first = std::stoi(m[1]);
                int second = std::stoi(m[2]);
                if (isValidDate(year, second, first)) {
                    return DateNumber(year, second, first, 0, 0, 0);
                } else if (isValidDate(year, first, second)) {
                    return DateNumber(year, first, second, 0, 0, 0);
                }
                return std::nan("");
            } },
        { std::wregex(L"(\\d{2})(\\d{2}|\\d{4})"), L"ddmmyy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                if (m.size() == 4) {
                    int year = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear)
                                         : century + std::stoi(m[3]);
                    return DateNumber(year, monthStringToNumber(m[2]), std::stoi(m[1]), 0, 0, 0);
                }
                return std::nan("");
            } },
        { std::wregex(L"(\\w{3})(\\d{2}|\\d{4})"), L"mmmyy",
            [withPivot, century](const std::wsmatch& m, int pivotYear) -> double {
                int year = withPivot ? std::stoi(m[2]) + pivotYear : century + std::stoi(m[2]);
                return DateNumber(year, monthStringToNumber(m[1]), 1, 0, 0, 0);
            } },
        { std::wregex(L"(\\w{3})[.]?(\\d{1,2})[,.]?(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"mmm.dd,yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\w{3})[.]?(\\d{1,2})[,.]?(\\d{4})"), L"mmm.dd,yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year = std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{4})/(\\d{1,2})/(\\d{1,2})"), L"yyyy/mm/dd",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), std::stoi(m[3]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{4})-(\\d{1,2})-(\\d{1,2})"), L"yyyy-mm-dd",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), std::stoi(m[3]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{4})-(\\d{1,2})-(\\d{1,2})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"yyyy-mm-dd HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), std::stoi(m[3]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\w{3})-(\\d{1,2})-(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"mmm-dd-yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\w{3})-(\\d{1,2})-(\\d{4})"), L"mmm-dd-yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})\\s(\\w{3})\\s(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"dd mmm yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[2]), std::stoi(m[1]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\d{1,2})\\s(\\w{3})\\s(\\d{4})"), L"dd mmm yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[2]), std::stoi(m[1]), 0, 0, 0);
            } },
        { std::wregex(L"(\\w{3})\\s(\\d{1,2})\\s(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"mmm dd yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\w{3})\\s(\\d{1,2})\\s(\\d{4})"), L"mmm dd yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year = parseYearWithPivot(std::stoi(m[3]), pivotYear);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})\\.(\\w{3})\\.(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"dd.mmm.yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[2]), std::stoi(m[1]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\d{1,2})\\.(\\w{3})\\.(\\d{4})"), L"dd.mmm.yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year = parseYearWithPivot(std::stoi(m[3]), pivotYear);
                return DateNumber(year, monthStringToNumber(m[2]), std::stoi(m[1]), 0, 0, 0);
            } },
        { std::wregex(L"(\\w{3})\\.(\\d{1,2})\\.(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"mmm.dd.yyyy HH:MM:SS",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year = std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\w{3})\\.(\\d{1,2})\\.(\\d{4})"), L"mmm.dd.yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})/(\\d{1,2})/(\\d{4})\\s(\\d{1,2}):(\\d{2})"), L"mm/dd/yyyy HH:MM",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(
                    year, std::stoi(m[1]), std::stoi(m[2]), std::stoi(m[4]), std::stoi(m[5]), 0);
            } },
        { std::wregex(L"(\\d{4})"), L"yyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, 1, 1, 0, 0, 0);
            } },
        { std::wregex(L"(\\d{4})-(\\d{2})"), L"yyyy-mm",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), 1, 0, 0, 0);
            } },
        { std::wregex(L"(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z"),
            L"yyyy-mm-ddTHH:MM:SSZ",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), std::stoi(m[3]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
        { std::wregex(L"(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})\\.(\\d{3})Z"),
            L"yyyy-mm-ddTHH:MM:SS.FFFZ",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[1]), pivotYear) : std::stoi(m[1]);
                return DateNumber(year, std::stoi(m[2]), std::stoi(m[3]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]), std::stoi(m[7]));
            } },
        { std::wregex(L"(\\w{3})[.]?(\\d{1,2})(\\d{4})"), L"mmm.ddyyyy",
            [withPivot](const std::wsmatch& m, int pivotYear) -> double {
                int year
                    = withPivot ? parseYearWithPivot(std::stoi(m[3]), pivotYear) : std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), 0, 0, 0);
            } },
        { std::wregex(L"(\\d{1,2})-(\\d{1,2}),\\s*(\\d{4})"), L"mm-dd, yyyy",
            [](const std::wsmatch& m, int pivotYear) -> double {
                int year = std::stoi(m[3]);
                int month = std::stoi(m[1]);
                int day = std::stoi(m[2]);
                return DateNumber(year, month, day, 0, 0, 0);
            } },
        { std::wregex(L"(\\d{2})(\\d{2})(\\d{4})"), L"ddmmyyyy",
            [](const std::wsmatch& m, int pivotYear) -> double {
                int day = std::stoi(m[1]);
                int month = std::stoi(m[2]);
                int year = std::stoi(m[3]);
                return DateNumber(year, month, day, 0, 0, 0);
            } },
        { std::wregex(L"(\\w{3})\\.(\\d{1,2})(\\d{4})\\s(\\d{1,2}):(\\d{2}):(\\d{2})"),
            L"mmm.ddyyyy HH:MM:SS",
            [](const std::wsmatch& m, int pivotYear) -> double {
                int year = std::stoi(m[3]);
                return DateNumber(year, monthStringToNumber(m[1]), std::stoi(m[2]), std::stoi(m[4]),
                    std::stoi(m[5]), std::stoi(m[6]));
            } },
    };
    return formats;
}
//=============================================================================
struct DateFormatCompiled
{
    // Auto-detect mode (empty format) tries every standard format; explicit mode
    // tries the candidates below, then the token-based fallback.
    bool autoDetect = false;
    bool withPivot = false;
    int pivotYear = 0;
    // Auto mode: the full standard format list. Explicit mode: the subset whose
    // format string equals the requested format (in list order).
    std::vector<DateFormatInfo> formats;
    // Explicit mode only: the requested format split into tokens for the
    // token-based fallback parser.
    std::vector<std::wstring> formatTokens;
    int currentYear = 1900;
    int century = 1900;
};
//=============================================================================
static void
getCurrentYearAndCentury(int& currentYear, int& century)
{
#ifdef _MSC_VER
    __time64_t rawtime;
    _time64(&rawtime);
    auto now = _localtime64(&rawtime);
#else
    time_t rawtime;
    time(&rawtime);
    struct tm t_result;
    auto now = localtime_r(&rawtime, &t_result);
#endif
    currentYear = now->tm_year + 1900;
    century = (currentYear / 100) * 100;
}
//=============================================================================
std::shared_ptr<DateFormatCompiled>
compileDateNumberFormat(const std::wstring& formatIn, bool withPivot, int pivotYear)
{
    auto compiled = std::make_shared<DateFormatCompiled>();
    compiled->autoDetect = formatIn.empty();
    compiled->withPivot = withPivot;
    compiled->pivotYear = pivotYear;
    getCurrentYearAndCentury(compiled->currentYear, compiled->century);

    std::vector<DateFormatInfo> allFormats = getDateFormatInfoList(withPivot);
    if (compiled->autoDetect) {
        compiled->formats = std::move(allFormats);
    } else {
        for (auto& format : allFormats) {
            if (format.format == formatIn) {
                compiled->formats.push_back(std::move(format));
            }
        }
        compiled->formatTokens = splitString(formatIn, L" -/:.,");
    }
    return compiled;
}
//=============================================================================
static double
parseWithTokenFallback(
    const std::wstring& datestring, const DateFormatCompiled& compiled, bool& bParsed)
{
    std::vector<std::wstring> dateTokens = splitString(datestring, L" -/:.,");
    const std::vector<std::wstring>& formatTokens = compiled.formatTokens;
    bool withPivot = compiled.withPivot;
    int pivotYear = compiled.pivotYear;
    int currentYear = compiled.currentYear;
    int century = compiled.century;

    int year = currentYear;
    int month = 1;
    int day = 1;
    int hours = 0;
    int minutes = 0;
    int secondes = 0;
    size_t tokenIndex = 0;
    bool timeFound = false;

    for (const auto& fmt : formatTokens) {
        if (fmt == L"AM" || fmt == L"PM") {
            bool findAmOrPm = false;
            for (auto s : dateTokens) {
                if (s == L"AM" || s == L"PM") {
                    findAmOrPm = true;
                }
            }
            if (!findAmOrPm) {
                bParsed = false;
                return std::nan("");
            }
        }
    }

    for (const auto& fmt : formatTokens) {
        if (tokenIndex >= dateTokens.size()) {
            break;
        }
        if (fmt == L"mmmyy") {
            try {
                std::wstring monthStr = datestring.substr(0, 3);
                std::wstring yearStr = datestring.substr(3, 2);

                int month = monthStringToNumber(monthStr);
                if (withPivot) {
                    year = parseYearWithPivot(std::stoi(yearStr), pivotYear);
                } else {
                    year = century + std::stoi(yearStr);
                }
                int day = 1;
                bParsed = true;
                return DateNumber(year, month, day);
            } catch (...) {
                bParsed = false;
                return std::nan("");
            }
        } else if (fmt == L"dd") {
            day = std::stoi(dateTokens[tokenIndex++]);
        } else if (fmt == L"mm") {
            month = std::stoi(dateTokens[tokenIndex++]);
        } else if (fmt == L"mmm") {
            month = monthStringToNumber(dateTokens[tokenIndex++]);
        } else if (fmt == L"yyyy") {
            year = std::stoi(dateTokens[tokenIndex++]);
        } else if (fmt == L"yy") {
            year = std::stoi(dateTokens[tokenIndex++]);
            if (withPivot) {
                year = (year < pivotYear ? year + 100 : year) - 1900;
            } else {
                year = century + year;
            }
        } else if (fmt == L"HH") {
            hours = std::stoi(dateTokens[tokenIndex++]);
            timeFound = true;
        } else if (fmt == L"MM") {
            minutes = std::stoi(dateTokens[tokenIndex++]);
            timeFound = true;
        } else if (fmt == L"SS") {
            secondes = std::stoi(dateTokens[tokenIndex++]);
            timeFound = true;
        } else {
            tokenIndex++;
        }
    }

    if (tokenIndex < dateTokens.size() && !timeFound) {
        std::wregex timeRegex(L"(\\d{1,2})(?::(\\d{1,2})(?::(\\d{1,2}))?)?");
        std::wstring remainingString = dateTokens[tokenIndex];
        std::wsmatch matches;
        if (std::regex_match(remainingString, matches, timeRegex)) {
            if (matches[1].matched) {
                int h = std::stoi(matches[1]);
                if (h < 24 && h > 0) {
                    hours = h;
                }
            }
            if (matches[2].matched) {
                int m = std::stoi(matches[2]);
                if (m < 60 && m > 0) {
                    minutes = m;
                }
            }
            if (matches[3].matched) {
                int s = std::stoi(matches[3]);
                if (s < 60 && s > 0) {
                    secondes = s;
                }
            }
        }
    }

    if (!isValidDate(year, month, day) || hours > 23 || minutes > 59 || secondes > 59) {
        bParsed = false;
        return std::numeric_limits<double>::quiet_NaN();
    }

    bParsed = true;

    return DateNumber(
        year, month, day, timeFound ? hours : 0, timeFound ? minutes : 0, timeFound ? secondes : 0);
}
//=============================================================================
double
DateNumber(const std::wstring& datestring, const DateFormatCompiled& compiled, bool& bParsed)
{
    for (const auto& format : compiled.formats) {
        std::wsmatch match;
        if (std::regex_match(datestring, match, format.regex)) {
            double value = format.parser(match, compiled.pivotYear);
            if (std::isfinite(value)) {
                bParsed = true;
                return value;
            }
        }
    }
    if (compiled.autoDetect) {
        bParsed = false;
        return std::nan("");
    }
    return parseWithTokenFallback(datestring, compiled, bParsed);
}
//=============================================================================
double
DateNumber(const std::wstring& datestring, const std::wstring& formatIn, bool withPivot,
    int pivotYear, bool& bParsed)
{
    std::shared_ptr<DateFormatCompiled> compiled
        = compileDateNumberFormat(formatIn, withPivot, pivotYear);
    return DateNumber(datestring, *compiled, bParsed);
}
//=============================================================================
double
DateNumber(const std::wstring& datestring, bool withPivot, int pivotYear, bool& bParsed)
{
    std::shared_ptr<DateFormatCompiled> compiled
        = compileDateNumberFormat(L"", withPivot, pivotYear);
    return DateNumber(datestring, *compiled, bParsed);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
