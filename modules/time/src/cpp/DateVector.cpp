//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DateVector.hpp"
#include "IEEEFP.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static double common_year[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
static double leap_year[] = { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 };
//=============================================================================
static void
normalizeDate(double& Y, double& M, double& D, double& H, double& MN, double& S, double& MS);
//=============================================================================
void
DateVector(double dateSerial, double& Y, double& M, double& D, double& H, double& MN, double& S,
    double& MS, bool rf)
{
    double ts = 0.;
    if (!IsFinite(dateSerial)) {
        Y = nan("");
        M = nan("");
        D = nan("");
        H = nan("");
        MN = nan("");
        S = nan("");
        MS = nan("");
        return;
    }
    if (dateSerial == floor(dateSerial)) {
        H = 0.;
        MN = 0.;
        S = 0.;
        MS = 0.;
    } else {
        dateSerial = 86400000 * dateSerial; // Convert to milliseconds
        if (rf) {
            dateSerial = floor(dateSerial + 0.5);
        }
        ts = dateSerial;
        dateSerial = floor(dateSerial / 1000.); // Convert to seconds
        MS = std::fmod(ts, 1000.); // Get milliseconds
        ts = dateSerial;
        dateSerial = floor(dateSerial / 60.);
        S = std::fmod(ts, 60.);
        ts = dateSerial;
        dateSerial = floor(dateSerial / 60.);
        MN = std::fmod(ts, 60.);
        ts = dateSerial;
        dateSerial = floor(dateSerial / 24.);
        H = std::fmod(ts, 24.);
    }
    dateSerial = floor(dateSerial);
    if (dateSerial == 0) {
        Y = 0.;
        M = 0.;
        D = 0.;
    } else {
        double y = floor(dateSerial / 365.2425);
        ts = dateSerial - (365.0 * y + ceil(0.25 * y) - ceil(0.01 * y) + ceil(0.0025 * y));
        if (ts <= 0) {
            y = y - 1.;
            dateSerial
                = dateSerial - (365.0 * y + ceil(0.25 * y) - ceil(0.01 * y) + ceil(0.0025 * y));
        } else {
            dateSerial = ts;
        }
        Y = y;
        int iy = static_cast<int>(y);
        int leap = static_cast<int>((iy % 4 == 0) && (iy % 100 != 0) || (iy % 400 == 0));
        double* cdm = (leap != 0 ? leap_year : common_year);
        int mon = static_cast<int>(dateSerial / 29. - 1);
        if (dateSerial > cdm[mon + 1]) {
            mon++;
        }
        M = mon + 1;
        dateSerial = dateSerial - cdm[mon];
        D = dateSerial;
    }
    normalizeDate(Y, M, D, H, MN, S, MS);
}
//=============================================================================
void
normalizeDate(double& Y, double& M, double& D, double& H, double& MN, double& S, double& MS)
{
    if (MS > 500) {
        S += 1;
        MS = 0;
    }
    if (S > 59) {
        MN += 1;
        S = 0;
    }
    if (MN > 59) {
        H += 1;
        MN = 0;
    }
    if (H > 23) {
        D += 1;
        H = 0;
    }

    // Define days in each month for common and leap years
    bool isLeapYear = (static_cast<int>(Y) % 4 == 0 && static_cast<int>(Y) % 100 != 0)
        || (static_cast<int>(Y) % 400 == 0);
    int daysInMonth[] = { 0, 31, isLeapYear ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

    // Adjust day overflow for the current month
    while (D > daysInMonth[static_cast<int>(M)]) {
        D -= daysInMonth[static_cast<int>(M)];
        M += 1;
    }

    // Adjust month overflow to increment the year
    while (M > 12) {
        M -= 12;
        Y += 1;

        // Recompute leap year status for the new year
        isLeapYear = (static_cast<int>(Y) % 4 == 0 && static_cast<int>(Y) % 100 != 0)
            || (static_cast<int>(Y) % 400 == 0);
        daysInMonth[2] = isLeapYear ? 29 : 28;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
