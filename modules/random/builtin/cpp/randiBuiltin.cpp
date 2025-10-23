//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "randiBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RandInteger.hpp"
#include "nlsBuildConfig.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::randiBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);

    // Default values
    indexType imin = 1;
    indexType imax = 1;
    Dimensions dims;
    NelsonType cl = NLS_DOUBLE;
    bool useLike = false;
    ArrayOf likeArray;

    sizeType nRhs = argIn.size();
    sizeType argIdx = 0;

    // Parse range argument
    if (argIdx < nRhs) {
        const ArrayOf& rangeArg = argIn[argIdx];
        if (rangeArg.isNumeric() && !rangeArg.isSparse()) {
            if (rangeArg.isRowVector() && rangeArg.getElementCount() == 2) {
                // [imin, imax]
                ArrayOf rangeVec = rangeArg;
                rangeVec.promoteType(NLS_DOUBLE);
                double* ptr = (double*)rangeVec.getDataPointer();
                imin = (indexType)ptr[0];
                imax = (indexType)ptr[1];
                argIdx++;
            } else if (rangeArg.isScalar()) {
                // imax
                imax = rangeArg.getContentAsScalarIndex();
                imin = 1;
                argIdx++;
            }
        }
    }

    // Parse size arguments
    if (argIdx < nRhs) {
        const ArrayOf& szArg = argIn[argIdx];
        if (szArg.isNumeric() && !szArg.isSparse()) {
            if (szArg.isRowVector() && szArg.getElementCount() > 0) {
                // randi(imax, [sz])
                ArrayOf szVec = szArg;
                szVec.promoteType(NLS_DOUBLE);
                double* ptr = (double*)szVec.getDataPointer();
                for (ompIndexType k = 0; k < szVec.getElementCount(); k++) {
                    dims[k] = (indexType)ptr[k];
                }
                argIdx++;
            } else if (szArg.isScalar()) {
                // randi(imax, n)
                indexType n = szArg.getContentAsScalarIndex();
                dims[0] = n;
                dims[1] = n;
                argIdx++;
            }
        }
    }

    // Parse additional dimension arguments
    for (; argIdx < nRhs; ++argIdx) {
        const ArrayOf& dimArg = argIn[argIdx];
        if (dimArg.isNumeric() && dimArg.isScalar()) {
            dims[dims.getLength()] = dimArg.getContentAsScalarIndex();
        } else if (dimArg.isRowVectorCharacterArray() || dimArg.isScalarStringArray()) {
            std::wstring paramstr = dimArg.getContentAsWideString();
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
                    cl = likeArray.getDataClass();
                    argIdx++; // skip next
                } else {
                    Error(_W("'like' must be followed by an array."));
                }
            } else {
                Error(_W("Unknown typename: ") + paramstr);
            }
        }
    }

    // Default dims if not set
    if (dims.getLength() == 0) {
        dims[0] = 1;
        dims[1] = 1;
    } else if (dims.getLength() == 1) {
        dims[1] = dims[0];
    }

    // Generate random integers
    ArrayOf result = RandInteger(imin, imax, dims, cl);
    retval << result;
    return retval;
}
//=============================================================================
