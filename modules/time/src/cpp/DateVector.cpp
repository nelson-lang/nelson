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
        double days = floor(dateSerial);
        double secondsOfDay = (dateSerial - days) * 86400.0;
        // Round to 0.1 millisecond to avoid '23:59:59.999999' artifacts from
        // binary floating-point noise, while keeping genuine fractional seconds.
        secondsOfDay = std::round(secondsOfDay * 10000.0) / 10000.0;
        if (rf) {
            secondsOfDay = std::round(secondsOfDay * 1000.0) / 1000.0;
        }
        if (secondsOfDay >= 86400.0) {
            days += 1.0;
            secondsOfDay = 0.0;
        }
        H = floor(secondsOfDay / 3600.0);
        secondsOfDay -= H * 3600.0;
        MN = floor(secondsOfDay / 60.0);
        secondsOfDay -= MN * 60.0;
        S = floor(secondsOfDay);
        MS = (secondsOfDay - S) * 1000.0;
        dateSerial = days;
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
}
//=============================================================================
} // namespace Nelson
//=============================================================================
