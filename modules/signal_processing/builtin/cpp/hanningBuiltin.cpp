//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "hanningBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SignalProcessingGateway::hanningBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isNumeric() || !argIn[0].isScalar() || argIn[0].isComplex()) {
        Error(L"Nelson:validators:mustBePositiveInteger", L"%s must be a positive integer.", L"M");
    }
    double mAsDouble = argIn[0].asDoubleScalar();
    if (!std::isfinite(mAsDouble) || mAsDouble < 0 || std::floor(mAsDouble) != mAsDouble) {
        Error(L"Nelson:validators:mustBePositiveInteger", L"%s must be a positive integer.", L"M");
    }
    bool periodic = false;
    if (argIn.size() == 2) {
        std::wstring option = argIn[1].asWideString();
        if (option == L"periodic") {
            periodic = true;
        } else if (option != L"symmetric") {
            Error(L"Nelson:hanning:invalidOption", L"\"periodic\" or \"symmetric\" expected.");
        }
    }
    indexType m = static_cast<indexType>(mAsDouble);
    Dimensions dims(m, 1);
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, m));
    if (m == 0) {
        ArrayOfVector retval;
        retval << ArrayOf(NLS_DOUBLE, dims, out, false);
        return retval;
    }
    if (m == 1) {
        out[0] = 1.;
    } else {
        double denominator = periodic ? static_cast<double>(m) : static_cast<double>(m - 1);
        const double twoPi = 2. * std::acos(-1.);
        OMP_PARALLEL_FOR_LOOP(m)
        for (ompIndexType k = 0; k < static_cast<ompIndexType>(m); ++k) {
            out[k] = 0.5 - 0.5 * std::cos(twoPi * static_cast<double>(k) / denominator);
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, dims, out, false);
    return retval;
}
//=============================================================================
