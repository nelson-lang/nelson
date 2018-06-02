//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "DateVector.hpp"
#include "IEEEFP.hpp"
#include <math.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static double common_year[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
static double leap_year[] = { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 };
//=============================================================================
void
DateVector(
    double dateSerial, double& Y, double& M, double& D, double& H, double& MN, double& S, bool rf)
{
    double ts = 0.;
    if (!IsFinite(dateSerial)) {
        Y = nan("");
        M = nan("");
        D = nan("");
        H = nan("");
        MN = nan("");
        S = nan("");
        return;
    }
    if (dateSerial == floor(dateSerial)) {
        H = 0.;
        MN = 0.;
        S = 0.;
    } else {
        dateSerial = 86400 * dateSerial;
        if (rf) {
            dateSerial = floor(dateSerial + 0.5);
        }
        ts = dateSerial;
        dateSerial = floor(dateSerial / 60.);
        S = ts - 60. * dateSerial;
        ts = dateSerial;
        dateSerial = floor(dateSerial / 60.);
        MN = ts - 60. * dateSerial;
        ts = dateSerial;
        dateSerial = floor(dateSerial / 24.);
        H = ts - 24. * dateSerial;
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
        int iy = (int)y;
        int leap = ((iy % 4 == 0) && (iy % 100 != 0) || (iy % 400 == 0));
        double* cdm = (leap ? leap_year : common_year);
        int mon = (int)(dateSerial / 29. - 1);
        if (dateSerial > cdm[mon + 1]) {
            mon++;
        }
        M = mon + 1;
        dateSerial = dateSerial - cdm[mon];
        D = dateSerial;
    }
}
}
//=============================================================================
