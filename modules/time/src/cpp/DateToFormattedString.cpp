//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
            Error(_W("Invalid date format (duplicated field): ") + key);
        }
    }

    if (mapCount[L"AM"] == 1 && mapCount[L"PM"] == 1) {
        Error(_W("Invalid date format (duplicated field): ") + L"AM/PM");
    }
}
//=============================================================================
std::wstring
formatDateTime(const std::wstring& format, const std::tm& dateTime, bool isLocalized)
{
    // format keys: order is important
    wstringVector keys = { L"AM", L"PM", L"yyyy", L"YYYY", L"yy", L"YY", L"QQ", L"mmmm", L"mmm",
        L"mm", L"m", L"dddd", L"ddd", L"dd", L"d", L"HH", L"MM", L"SS", L"FFF" };

    checkDateFormatIsValid(keys, format);

    std::wstring result = format;
    bool ignoreD = false;
    bool ignoreY = false;
    bool ignoreM = false;
    bool isAM = false;
    bool isPM = false;
    bool replaceAmByPM = false;

    for (const auto& key : keys) {
        size_t pos = result.find(key);
        while (pos != std::wstring::npos) {
            std::wstringstream replacement;
            if ((key == L"yyyy" || key == L"YYYY") && !ignoreY) {
                ignoreY = true;
                replacement << std::setw(4) << dateTime.tm_year + 1900;
            } else if ((key == L"yy" || key == L"YY") && !ignoreY) {
                ignoreY = true;
                replacement << std::setw(2) << std::setfill(L'0')
                            << (dateTime.tm_year + 1900) % 100;
            } else if (key == L"mm") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_mon + 1;
            } else if (key == L"dd" && !ignoreD) {
                ignoreD = true;
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_mday;
            } else if (key == L"HH") {
                int hour = dateTime.tm_hour;
                if (isAM && dateTime.tm_hour > 12) {
                    hour -= 12;
                    replacement << std::setw(2) << std::setfill(L' ') << hour;
                    replaceAmByPM = true;
                } else if (isAM || isPM) {
                    if (hour == 0) {
                        hour = 12;
                    } else if (hour > 12) {
                        hour -= 12;
                    }
                    if (hour > 9) {
                        replacement << std::setw(2) << std::setfill(L'0') << hour;
                    } else {
                        replacement << std::setw(2) << std::setfill(L' ') << hour;
                    }
                } else {
                    replacement << std::setw(2) << std::setfill(L'0') << hour;
                }

            } else if (key == L"MM") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_min;
            } else if (key == L"SS") {
                replacement << std::setw(2) << std::setfill(L'0') << dateTime.tm_sec;
            } else if (key == L"QQ") {
                int quarter = (dateTime.tm_mon / 3) + 1;
                replacement << L"Q" << quarter;
            } else if ((key == L"mmmm" || key == L"mmm" || key == L"m") && !ignoreM) {
                ignoreM = true;
                std::wstring monthNames[]
                    = { L"January", L"February", L"March", L"April", L"May", L"June", L"July",
                          L"August", L"September", L"October", L"November", L"December" };
                std::wstring month
                    = isLocalized ? _W(monthNames[dateTime.tm_mon]) : monthNames[dateTime.tm_mon];
                if (key == L"m") {
                    replacement << month.substr(0, 1);
                } else if (key == L"mmm") {
                    replacement << month.substr(0, 3);
                } else {
                    replacement << month;
                }
            } else if ((key == L"dddd" || key == L"ddd" || key == L"d") && !ignoreD) {
                ignoreD = true;
                std::wstring dayNames[] = { L"Saturday", L"Sunday", L"Monday", L"Tuesday",
                    L"Wednesday", L"Thursday", L"Friday" };
                std::wstring day
                    = isLocalized ? _W(dayNames[dateTime.tm_wday]) : dayNames[dateTime.tm_wday];
                if (key == L"d") {
                    replacement << day.substr(0, 1);
                } else if (key == L"ddd") {
                    replacement << day.substr(0, 3);
                } else {
                    replacement << day;
                }
            } else if (key == L"FFF") {
                replacement << std::setw(3) << std::setfill(L'0') << 0;
            } else if (key == L"AM" || key == L"PM") {
                if (key == L"AM") {
                    isAM = true;
                    isPM = false;
                } else {
                    isPM = true;
                    isAM = false;
                }
            } else {
                replacement << L"";
            }
            if (replacement.str().empty()) {
                pos = std::wstring::npos;
            } else {
                result.replace(pos, key.length(), replacement.str());
                pos = result.find(key);
            }
        }
    }
    if (replaceAmByPM) {
        size_t pos = result.find(L"AM");
        if (pos != std::wstring::npos) {
            result.replace(pos, 2, L"PM");
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
epochToUserFormatDateTimeString(
    int Y, int M, int D, int H, int MN, int S, const std::wstring& userFormat, bool isLocalized)
{
    std::tm dateTime = {};
    dateTime.tm_year = Y - 1900;
    dateTime.tm_mon = M - 1;
    dateTime.tm_mday = D;
    dateTime.tm_hour = H;
    dateTime.tm_min = MN;
    dateTime.tm_sec = S;
    dateTime.tm_wday = getWeekday(Y, M, D);
    return formatDateTime(userFormat, dateTime, isLocalized);
}
//=============================================================================
std::wstring
epochToUserFormatDateTimeString(double dateSerial, const std::wstring& userFormat, bool isLocalized)
{
    double Y, M, D, H, MN, S;
    DateVector(dateSerial, Y, M, D, H, MN, S, true);
    return epochToUserFormatDateTimeString(Y, M, D, H, MN, S, userFormat, isLocalized);
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

    indexType rows = dimensionsIn.getRows();
    indexType columns = dimensionsIn.getColumns();

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
            if (!hasDateVector)
                break;
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
                results[l++]
                    = epochToUserFormatDateTimeString(Y, M, D, H, MN, S, userFormat, isLocalized);
            }
        }
    } else {
        results.resize(epochValues.size());
        bool forceFullFormat = !areAllIntegers(epochValues);
        for (size_t k = 0; k < epochValues.size(); ++k) {
            double dateSerial = epochValues[k];
            results[k] = epochToUserFormatDateTimeString(dateSerial, userFormat, isLocalized);
        }
    }
    return results;
}
//=============================================================================
}
//=============================================================================
