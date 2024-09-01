//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "DateString.hpp"
#include "characters_encoding.hpp"
#include "DateVector.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const std::vector<std::wstring> weekday_abbr
    = { L"Sat", L"Sun", L"Mon", L"Tue", L"Wed", L"Thu", L"Fri" };
//=============================================================================
static const std::vector<std::wstring> month_abbr = { L"Jan", L"Feb", L"Mar", L"Apr", L"May",
    L"Jun", L"Jul", L"Aug", L"Sep", L"Oct", L"Nov", L"Dec" };
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
static int
getMonthIndex(int M)
{
    if (M < 1 || M > 12) {
        return 0;
    }
    return M - 1;
}
//=============================================================================
static std::wstring
getMonthString(int month_index, bool isLocalized)
{
    return isLocalized ? _W(month_abbr[month_index]) : month_abbr[month_index];
}
//=============================================================================
static std::wstring
getWeekdayString(int Y, int M, int D, bool isLocalized)
{
    int dow = getWeekday(Y, M, D);
    return isLocalized ? _W(weekday_abbr[dow]) : weekday_abbr[dow];
}
//=============================================================================
std::wstring
selectDateFormat(int predefinedFormatOutValue, int H, int MN, int S, bool forceFullFormat)
{
    switch (predefinedFormatOutValue) {
    case -1:
        return (H == 0 && MN == 0 && S == 0 && !forceFullFormat) ? L"%02d-%s-%04d"
                                                                 : L"%02d-%s-%04d %02d:%02d:%02d";
    case 0:
        return L"%02d-%s-%04d %02d:%02d:%02d";
    case 1:
        return L"%02d-%s-%04d";
    case 2:
        return L"%02d/%02d/%02d";
    case 3:
        return L"%s";
    case 4:
        return L"%c";
    case 5:
        return L"%02d";
    case 6:
        return L"%02d/%02d";
    case 7:
        return L"%02d";
    case 8:
        return L"%s";
    case 9:
        return L"%c";
    case 10:
        return L"%04d";
    case 11:
        return L"%02d";
    case 12:
        return L"%s%02d";
    case 13:
        return L"%02d:%02d:%02d";
    case 14:
        return L" %d:%02d:%02d %s";
    case 15:
        return L"%02d:%02d";
    case 16:
        return L" %d:%02d %s";
    case 17:
        return L"Q%d-%02d";
    case 18:
        return L"Q%d";
    case 19:
        return L"%02d/%02d";
    case 20:
        return L"%02d/%02d/%02d";
    case 21:
        return L"%s.%02d,%04d %02d:%02d:%02d";
    case 22:
        return L"%s.%02d,%04d";
    case 23:
        return L"%02d/%02d/%04d";
    case 24:
        return L"%02d/%02d/%04d";
    case 25:
        return L"%02d/%02d/%02d";
    case 26:
        return L"%04d/%02d/%02d";
    case 27:
        return L"Q%d-%04d";
    case 28:
        return L"%s%04d";
    case 29:
        return L"%04d-%02d-%02d";
    case 30:
        return L"%04d%02d%02dT%02d%02d%02d";
    case 31:
        return L"%04d-%02d-%02d %02d:%02d:%02d";
    default:
        return L"%02d-%s-%04d %02d:%02d:%02d";
    }
}
//=============================================================================
static std::wstring
formatDateTimeString(int Y, int M, int D, int H, int MN, int S, const std::wstring& dateFormat,
    const std::wstring& month_str, const std::wstring& weekday_str, int predefinedFormatOutValue)
{
    auto getAMPM = [](int hour) -> std::wstring { return (hour < 12) ? L"AM" : L"PM"; };
    auto to12HourFormat
        = [](int hour) -> int { return (hour == 0 || hour == 12) ? 12 : hour % 12; };
    auto getQuarter = [](int month) -> int { return (month - 1) / 3 + 1; };

    switch (predefinedFormatOutValue) {
    case 2:
    case 5:
        return fmt::sprintf(dateFormat, M, D, Y % 100);
    case 19:
        return fmt::sprintf(dateFormat, D, M);
    case 20:
        return fmt::sprintf(dateFormat, D, M, Y % 100);
    case 23:
        return fmt::sprintf(dateFormat, M, D, Y);
    case 24:
        return fmt::sprintf(dateFormat, D, M, Y);
    case 25:
        return fmt::sprintf(dateFormat, Y % 100, M, D);
    case 3:
    case 12:
        return fmt::sprintf(dateFormat, month_str, Y % 100);
    case 4:
        return fmt::sprintf(dateFormat, std::toupper(month_str[0]));
    case 8:
        return fmt::sprintf(dateFormat, weekday_str);
    case 9:
        return fmt::sprintf(dateFormat, std::toupper(weekday_str[0]));
    case 10:
        return fmt::sprintf(dateFormat, Y);
    case 11: {
        std::wstring res = fmt::sprintf(dateFormat, Y);
        return res.substr(res.length() - 2);
    }
    case 13:
        return fmt::sprintf(dateFormat, H, MN, S);
    case 14:
        return fmt::sprintf(dateFormat, to12HourFormat(H), MN, S, getAMPM(H));
    case 16:
        return fmt::sprintf(dateFormat, to12HourFormat(H), MN, getAMPM(H));
    case 15:
        return fmt::sprintf(dateFormat, H, MN);
    case 17:
    case 18:
        return fmt::sprintf(dateFormat, getQuarter(M), Y % 100);
    case 21:
        return fmt::sprintf(dateFormat, month_str, D, Y, H, MN, S);
    case 22:
        return fmt::sprintf(dateFormat, month_str, D, Y);
    case 28:
        return fmt::sprintf(dateFormat, month_str, Y);
    case 26:
    case 29:
    case 30:
    case 31:
        return fmt::sprintf(dateFormat, Y, M, D, H, MN, S);
    case 27:
        return fmt::sprintf(dateFormat, getQuarter(M), Y);
    case 6:
        return fmt::sprintf(dateFormat, M, D);
    default:
        return fmt::sprintf(dateFormat, D, month_str, Y, H, MN, S);
    }
}
//=============================================================================
std::wstring
epochToDateTimeString(int Y, int M, int D, int H, int MN, int S, bool forceFullFormat,
    bool isLocalized, int predefinedFormatOutValue)
{
    int month_index = getMonthIndex(M);
    const std::wstring& month_str = getMonthString(month_index, isLocalized);
    const std::wstring& weekday_str = getWeekdayString(Y, M, D, isLocalized);

    std::wstring dateFormat = selectDateFormat(predefinedFormatOutValue, H, MN, S, forceFullFormat);

    return formatDateTimeString(
        Y, M, D, H, MN, S, dateFormat, month_str, weekday_str, predefinedFormatOutValue);
}
//=============================================================================
std::wstring
epochToDateTimeString(
    double dateSerial, bool forceFullFormat, bool isLocalized, int predefinedFormatOutValue)
{
    double Y, M, D, H, MN, S;
    DateVector(dateSerial, Y, M, D, H, MN, S, true);
    return epochToDateTimeString(
        Y, M, D, H, MN, S, forceFullFormat, isLocalized, predefinedFormatOutValue);
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
DateString(std::vector<double> epochValues, const Dimensions& dimensionsIn, bool isLocalized,
    int predefinedFormatOutValue)
{
    wstringVector results;

    if (dimensionsIn.isEmpty(false)) {
        return results;
    }

    bool hasDateVector = false;
    bool noHMNS = true;

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

                if (H != 0 || MN != 0 || S != 0) {
                    noHMNS = false;
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
                results[l++] = epochToDateTimeString(
                    Y, M, D, H, MN, S, !noHMNS, isLocalized, predefinedFormatOutValue);
            }
        }
    } else {
        results.resize(epochValues.size());
        bool forceFullFormat = !areAllIntegers(epochValues);
        for (size_t k = 0; k < epochValues.size(); ++k) {
            results[k] = epochToDateTimeString(
                epochValues[k], forceFullFormat, isLocalized, predefinedFormatOutValue);
        }
    }
    return results;
} //=============================================================================
} // namespace Nelson
//=============================================================================
