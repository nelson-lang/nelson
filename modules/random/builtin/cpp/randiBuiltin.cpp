//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "randiBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "RandInteger.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
validateRandiLimits(double iminAsDouble, double imaxAsDouble, bool scalarForm)
{
    bool valid = std::isfinite(iminAsDouble) && std::isfinite(imaxAsDouble)
        && (std::floor(iminAsDouble) == iminAsDouble) && (std::floor(imaxAsDouble) == imaxAsDouble)
        && (iminAsDouble <= imaxAsDouble);
    if (valid && scalarForm) {
        valid = imaxAsDouble >= 1;
    }
    if (!valid) {
        Error(L"Nelson:randi:invalidRange",
            L"First input must be a positive scalar integer value IMAX, or two integer values "
            L"[IMIN IMAX] with IMIN less than or equal to IMAX.");
    }
    const double LIMIT_2_POW_53 = 9007199254740992.; // 2^53
    if (std::abs(iminAsDouble) >= LIMIT_2_POW_53 || std::abs(imaxAsDouble) >= LIMIT_2_POW_53) {
        Error(L"Nelson:randi:rangeTooLarge", L"IMIN and IMAX must be less than 2^53.");
    }
}
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::randiBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);

    // Default values
    int64 imin = 1;
    int64 imax = 1;
    Dimensions dims;
    NelsonType cl = NLS_DOUBLE;
    bool useLike = false;
    ArrayOf likeArray;

    sizeType nRhs = argIn.size();
    sizeType argIdx = 0;

    if (argIdx < nRhs) {
        const ArrayOf& rangeArg = argIn[argIdx];
        if (!(rangeArg.isNumeric() || rangeArg.dataClass() == NLS_LOGICAL) || rangeArg.isSparse()
            || rangeArg.isComplex()
            || !(rangeArg.isScalar() || (rangeArg.isRowVector() && rangeArg.elementCount() == 2))) {
            Error(L"Nelson:randi:invalidRange",
                L"First input must be a positive scalar integer value IMAX, or two integer values "
                L"[IMIN IMAX] with IMIN less than or equal to IMAX.");
        }
        {
            if (rangeArg.isRowVector() && rangeArg.elementCount() == 2) {
                // [imin, imax]
                ArrayOf rangeVec = rangeArg;
                rangeVec.promoteType(NLS_DOUBLE);
                double* ptr = (double*)rangeVec.dataPointer();
                validateRandiLimits(ptr[0], ptr[1], false);
                imin = (int64)ptr[0];
                imax = (int64)ptr[1];
                argIdx++;
            } else if (rangeArg.isScalar()) {
                // imax
                ArrayOf rangeVec = rangeArg;
                rangeVec.promoteType(NLS_DOUBLE);
                double imaxAsDouble = ((double*)rangeVec.dataPointer())[0];
                validateRandiLimits(1, imaxAsDouble, true);
                imax = (int64)imaxAsDouble;
                imin = 1;
                argIdx++;
            }
        }
    }

    if (argIdx < nRhs) {
        const ArrayOf& szArg = argIn[argIdx];
        if ((szArg.isNumeric() || szArg.dataClass() == NLS_LOGICAL) && !szArg.isSparse()) {
            if (szArg.isEmpty()) {
                // randi(imax, []) is empty
                dims[0] = 0;
                dims[1] = 0;
                argIdx++;
            } else if (szArg.isRowVector() && szArg.elementCount() > 1) {
                // randi(imax, [sz])
                ArrayOf szVec = szArg;
                szVec.promoteType(NLS_DOUBLE);
                double* ptr = (double*)szVec.dataPointer();
                for (ompIndexType k = 0; k < (ompIndexType)szVec.elementCount(); k++) {
                    if (!std::isfinite(ptr[k]) || std::floor(ptr[k]) != ptr[k]) {
                        Error(L"Nelson:integerValueExpected",
                            L"A real integer value scalar expected.");
                    }
                    dims[k] = (ptr[k] > 0) ? (indexType)ptr[k] : 0;
                }
                argIdx++;
            } else if (szArg.isScalar()) {
                // randi(imax, n) or randi(imax, n, m, ...)
                dims[0] = szArg.asScalarIndex(true, true, true);
                argIdx++;
            }
        }
    }

    for (; argIdx < nRhs; ++argIdx) {
        const ArrayOf& dimArg = argIn[argIdx];
        if ((dimArg.isNumeric() || dimArg.dataClass() == NLS_LOGICAL) && dimArg.isScalar()) {
            dims[dims.length()] = dimArg.asScalarIndex(true, true, true);
        } else if (dimArg.isRowVectorCharacterArray() || dimArg.isScalarStringArray()) {
            std::wstring paramstr = dimArg.asWideString();
            if (paramstr == L"double") {
                cl = NLS_DOUBLE;
            } else if (paramstr == L"single") {
                cl = NLS_SINGLE;
            } else if (paramstr == L"int8") {
                cl = NLS_INT8;
            } else if (paramstr == L"uint8") {
                cl = NLS_UINT8;
            } else if (paramstr == L"int16") {
                cl = NLS_INT16;
            } else if (paramstr == L"uint16") {
                cl = NLS_UINT16;
            } else if (paramstr == L"int32") {
                cl = NLS_INT32;
            } else if (paramstr == L"uint32") {
                cl = NLS_UINT32;
            } else if (paramstr == L"logical") {
                cl = NLS_LOGICAL;
            } else if (paramstr == L"like") {
                useLike = true;
                if (argIdx + 1 < nRhs) {
                    likeArray = argIn[argIdx + 1];
                    cl = likeArray.dataClass();
                    argIdx++;
                } else {
                    Error(L"Nelson:randi:invalidLike", L"'like' must be followed by an array.");
                }
            } else {
                Error(L"Nelson:randi:unknownTypename", L"Unknown typename: %s.", paramstr);
            }
        }
    }

    if (dims.length() == 0) {
        dims[0] = 1;
        dims[1] = 1;
    } else if (dims.length() == 1) {
        dims[1] = dims[0];
    }

    ArrayOf result = RandInteger(imin, imax, dims, cl);
    retval << result;
    return retval;
}
//=============================================================================
