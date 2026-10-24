//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include <string>
#include <iomanip>
#include "DateToFormattedString.hpp"
#include "DateVector.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
checkDateFormatIsValid(const wstringVector& keys, const std::wstring& format)
{
    std::map<std::wstring, int> mapCount;
    for (const auto& key : keys) {
        mapCount[key] = 0;
    }

    for (const auto& key : keys) {
        bool skip = false;
        if (key == L"yy") {
            if (mapCount[L"yyyy"] == 0 && mapCount[L"YYYY"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"YY") {
            if (mapCount[L"yyyy"] == 0 && mapCount[L"YYYY"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"mmm") {
            if (mapCount[L"mmmm"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"mm") {
            if (mapCount[L"mmm"] == 0 && mapCount[L"mmmm"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"m") {
            if (mapCount[L"mm"] == 0 && mapCount[L"mmm"] == 0 && mapCount[L"mmmm"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }
        if (key == L"ddd") {
            if (mapCount[L"dddd"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"dd") {
            if (mapCount[L"dddd"] == 0 && mapCount[L"ddd"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (key == L"d") {
            if (mapCount[L"dddd"] == 0 && mapCount[L"ddd"] == 0 && mapCount[L"dd"] == 0) {
                mapCount[key] = StringHelpers::count(format, key);
            } else {
                skip = true;
            }
        }

        if (!skip) {
            mapCount[key] = StringHelpers::count(format, key);
        }
    }

    for (const auto& key : keys) {
        if (mapCount[key] > 1) {
            Error(L"Nelson:datestr:duplicatedField", L"Invalid date format (duplicated field): %s",
                key);
        }
    }

    if (mapCount[L"AM"] == 1 && mapCount[L"PM"] == 1) {
        Error(L"Nelson:datestr:duplicatedField", L"Invalid date format (duplicated field): %s",
            std::wstring(L"AM/PM"));
    }
}
//=============================================================================
static std::wstring
formatDateTime(
    const std::wstring& format, const std::tm& dateTime, int milliseconds, bool isLocalized)
{
    // Validation keys: order is important for the duplicated-field diagnostics.
    wstringVector keys = { L"AM", L"PM", L"yyyy", L"YYYY", L"yy", L"YY", L"QQ", L"mmmm", L"mmm",
        L"mm", L"m", L"dddd", L"ddd", L"dd", L"d", L"HH", L"MM", L"SS", L"FFF" };

    checkDateFormatIsValid(keys, format);

    // Longest-match token order for a single left-to-right scan. The output is
    // never re-scanned, so substituted names (the 'd' in "Tuesday") cannot
    // corrupt tokens that appear later in the format.
    static const wstringVector tokens = { L"yyyy", L"YYYY", L"dddd", L"mmmm", L"FFF", L"mmm",
        L"ddd", L"yy", L"YY", L"QQ", L"AM", L"PM", L"HH", L"MM", L"SS", L"mm", L"dd", L"m", L"d" };

    // The presence of a meridiem token (AM/PM) switches HH to a 12-hour clock.
    bool use12Hour
        = (format.find(L"AM") != std::wstring::npos) || (format.find(L"PM") != std::wstring::npos);

    const std::wstring monthNames[] = { L"January", L"February", L"March", L"April", L"May",
        L"June", L"July", L"August", L"September", L"October", L"November", L"December" };
    const std::wstring dayNames[]
        = { L"Saturday", L"Sunday", L"Monday", L"Tuesday", L"Wednesday", L"Thursday", L"Friday" };

    std::wstring result;
    size_t i = 0;
    while (i < format.size()) {
        bool matched = false;
        for (const auto& key : tokens) {
            if (format.compare(i, key.length(), key) != 0) {
                continue;
            }
            matched = true;
            std::wstringstream replacement;
            if (key == L"yyyy" || key == L"YYYY") {
                replacement << std::setw(4) << dateTime.tm_year + 1900;
            } else if (key == L"yy" || key == L"YY") {
                replacement << std::setw(2) << std::setfill(L'0')
                            << (dateTime.tm_year + 1900) % 100;
            } else if (key == L"mmmm" || key == L"mmm" || key == L"m") {
                std::wstring month
                    = isLocalized ? _W(monthNames[dateTime.tm_mon]) : monthNames[dateTime.tm_mon];
                if (key == L"m") {
                    replacement << month.substr(0, 1);
                } else if (key == L"mmm") {
                    replacement << month.substr(0, 3);
                } else {
                    replacement << month;
                }
            } else if (key == L"mm") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_mon + 1;
            } else if (key == L"dddd" || key == L"ddd" || key == L"d") {
                std::wstring day
                    = isLocalized ? _W(dayNames[dateTime.tm_wday]) : dayNames[dateTime.tm_wday];
                if (key == L"d") {
                    replacement << day.substr(0, 1);
                } else if (key == L"ddd") {
                    replacement << day.substr(0, 3);
                } else {
                    replacement << day;
                }
            } else if (key == L"dd") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_mday;
            } else if (key == L"HH") {
                if (use12Hour) {
                    int hour = dateTime.tm_hour % 12;
                    if (hour == 0) {
                        hour = 12;
                    }
                    // Single-digit 12-hour values are space-padded (" 1", not "01").
                    replacement << std::setw(2) << std::setfill(L' ') << hour;
                } else {
                    replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_hour;
                }
            } else if (key == L"MM") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_min;
            } else if (key == L"SS") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_sec;
            } else if (key == L"QQ") {
                int quarter = (dateTime.tm_mon / 3) + 1;
                replacement << L"Q" << quarter;
            } else if (key == L"FFF") {
                replacement << std::setw(3) << std::setfill(L'0') << milliseconds;
            } else if (key == L"AM" || key == L"PM") {
                // The AM/PM literal is a placeholder replaced by the actual meridiem.
                replacement << (dateTime.tm_hour < 12 ? L"AM" : L"PM");
            }
            result += replacement.str();
            i += key.length();
            break;
        }
        if (!matched) {
            result += format[i];
            ++i;
        }
    }

    return result;
}
//=============================================================================
static int
getWeekday(int year, int month, int day)
{
    if (month == 1) {
        month = 13;
        year--;
    }
    if (month == 2) {
        month = 14;
        year--;
    }
    int q = day;
    int m = month;
    int k = year % 100;
    int j = year / 100;
    int h = q + 13 * (m + 1) / 5 + k + k / 4 + j / 4 + 5 * j;
    h = h % 7;
    return h;
}
//=============================================================================
std::wstring
epochToUserFormatDateTimeString(int Y, int M, int D, int H, int MN, int S, int MS,
    const std::wstring& userFormat, bool isLocalized)
{
    std::tm dateTime = {};
    dateTime.tm_year = Y - 1900;
    dateTime.tm_mon = M - 1;
    dateTime.tm_mday = D;
    dateTime.tm_hour = H;
    dateTime.tm_min = MN;
    dateTime.tm_sec = S;
    dateTime.tm_wday = getWeekday(Y, M, D);
    return formatDateTime(userFormat, dateTime, MS, isLocalized);
}
//=============================================================================
std::wstring
epochToUserFormatDateTimeString(double dateSerial, const std::wstring& userFormat, bool isLocalized)
{
    double Y, M, D, H, MN, S, MS;
    DateVector(dateSerial, Y, M, D, H, MN, S, MS, false);
    return epochToUserFormatDateTimeString(Y, M, D, H, MN, S, MS, userFormat, isLocalized);
}
//=============================================================================
static bool
areAllIntegers(const std::vector<double>& epochValues)
{
    for (const double& value : epochValues) {
        if (std::floor(value) != value) {
            return false;
        }
    }
    return true;
}
//=============================================================================
wstringVector
dateToUserFormatString(std::vector<double> epochValues, const Dimensions& dimensionsIn,
    const std::wstring& userFormat, bool isLocalized)
{
    wstringVector results = {};
    if (dimensionsIn.isEmpty(false)) {
        return results;
    }

    bool hasDateVector = false;

    indexType rows = dimensionsIn.rows();
    indexType columns = dimensionsIn.columns();

    if ((columns % 6) == 0) {
        hasDateVector = true;
        for (indexType i = 0; i < rows; ++i) {
            for (indexType j = 0; j < columns; j += 6) {
                int Y = static_cast<int>(epochValues[i + (j * rows)]);
                int M = static_cast<int>(epochValues[i + ((j + 1) * rows)]);
                int D = static_cast<int>(epochValues[i + ((j + 2) * rows)]);
                int H = static_cast<int>(epochValues[i + ((j + 3) * rows)]);
                int MN = static_cast<int>(epochValues[i + ((j + 4) * rows)]);
                int S = static_cast<int>(epochValues[i + ((j + 5) * rows)]);
                std::vector<double> dateVector
                    = { static_cast<double>(Y), static_cast<double>(M), static_cast<double>(D),
                          static_cast<double>(H), static_cast<double>(MN), static_cast<double>(S) };

                if (!areAllIntegers(dateVector) || M < 1 || M > 12 || D < 1 || D > 31 || H < 0
                    || H > 23 || MN < 0 || MN > 59 || S < 0 || S > 59) {
                    hasDateVector = false;
                    break;
                }
            }
            if (!hasDateVector) {
                break;
            }
        }
    }
    if (hasDateVector) {
        results.resize(rows * (columns / 6));
        indexType l = 0;
        for (indexType i = 0; i < rows; ++i) {
            for (indexType j = 0; j < columns; j += 6) {
                int Y = static_cast<int>(epochValues[i + (j * rows)]);
                int M = static_cast<int>(epochValues[i + ((j + 1) * rows)]);
                int D = static_cast<int>(epochValues[i + ((j + 2) * rows)]);
                int H = static_cast<int>(epochValues[i + ((j + 3) * rows)]);
                int MN = static_cast<int>(epochValues[i + ((j + 4) * rows)]);
                int S = static_cast<int>(epochValues[i + ((j + 5) * rows)]);
                results[l++] = epochToUserFormatDateTimeString(
                    Y, M, D, H, MN, S, 0, userFormat, isLocalized);
            }
        }
    } else {
        results.resize(epochValues.size());
        for (size_t k = 0; k < epochValues.size(); ++k) {
            double dateSerial = epochValues[k];
            results[k] = epochToUserFormatDateTimeString(dateSerial, userFormat, isLocalized);
        }
    }
    return results;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
