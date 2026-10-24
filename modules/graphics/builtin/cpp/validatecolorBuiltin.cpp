//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "validatecolorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOColorHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
validateTextColors(const ArrayOf& param1, bool isMultiple);
//=============================================================================
template <class T>
ArrayOf
validateRealColor(NelsonType nlsType, const ArrayOf param1, bool isMultiple)
{
    ArrayOf res;
    if (param1.columns() != 3 || param1.isEmpty()) {
        Error(L"Nelson:graphics:validatecolor:InvalidColor", L"Valid color expected.");
    }
    if (param1.rows() > 1 && !isMultiple) {
        Error(L"Nelson:graphics:validatecolor:MultipleColors",
            L"One color specification or use the 'multiple' option expected.");
    }
    if (param1.rows() == 1) {
        Dimensions dimsC(param1.rows(), 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.elementCount()));
        res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        std::vector<double> rgb;
        if (!ParseColorToRGB(param1, rgb)) {
            Error(L"Nelson:graphics:validatecolor:InvalidColor", L"Valid color expected.");
        }
        memcpy(dptr, rgb.data(), 3 * sizeof(double));
    } else {
        Dimensions dimsC(param1.rows(), 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.elementCount()));
        res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        const T* values = static_cast<const T*>(param1.dataPointer());
        for (indexType k = 0; k < param1.rows(); ++k) {
            Dimensions dimsArg(1, 3);
            T* uptr = static_cast<T*>(ArrayOf::allocateArrayOf(nlsType, 3));
            ArrayOf arg = ArrayOf(nlsType, dimsArg, uptr);
            uptr[0] = values[k];
            uptr[1] = values[k + (param1.rows())];
            uptr[2] = values[k + (2 * param1.rows())];
            std::vector<double> rgb;
            if (!ParseColorToRGB(arg, rgb)) {
                Error(L"Nelson:graphics:validatecolor:InvalidColor", L"Valid color expected.");
            }
            dptr[k] = rgb[0];
            dptr[k + (param1.rows())] = rgb[1];
            dptr[k + (2 * param1.rows())] = rgb[2];
        }
    }
    return res;
}
//=============================================================================
ArrayOfVector
GraphicsGateway::validatecolorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);

    bool isMultiple = false;
    if (argIn.size() == 1) {
        isMultiple = false;
    } else {
        std::wstring param2 = argIn[1].asWideString();
        if (param2 == L"one") {
            isMultiple = false;
        } else if (param2 == L"multiple") {
            isMultiple = true;
        } else {
            Error("Nelson:graphics:validatecolor:InvalidOption",
                "Invalid #%d argument: 'one', 'multiple' expected.", 2);
        }
    }
    ArrayOf param1 = argIn[0];
    switch (param1.dataClass()) {
    case NLS_UINT8: {
        retval << validateRealColor<uint8>(NLS_UINT8, param1, isMultiple);
    } break;
    case NLS_INT8: {
        retval << validateRealColor<int8>(NLS_INT8, param1, isMultiple);
    } break;
    case NLS_UINT16: {
        retval << validateRealColor<uint16>(NLS_UINT16, param1, isMultiple);
    } break;
    case NLS_INT16: {
        retval << validateRealColor<int16>(NLS_INT16, param1, isMultiple);
    } break;
    case NLS_UINT32: {
        retval << validateRealColor<uint32>(NLS_UINT32, param1, isMultiple);
    } break;
    case NLS_INT32: {
        retval << validateRealColor<int32>(NLS_INT32, param1, isMultiple);
    } break;
    case NLS_UINT64: {
        retval << validateRealColor<uint64>(NLS_UINT64, param1, isMultiple);
    } break;
    case NLS_INT64: {
        retval << validateRealColor<int64>(NLS_INT64, param1, isMultiple);
    } break;
    case NLS_SINGLE: {
        retval << validateRealColor<single>(NLS_SINGLE, param1, isMultiple);
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf sparam1(param1);
        sparam1.promoteType(NLS_SINGLE);
        retval << validateRealColor<single>(NLS_SINGLE, sparam1, isMultiple);
    } break;
    case NLS_DOUBLE: {
        retval << validateRealColor<double>(NLS_DOUBLE, param1, isMultiple);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf dparam1(param1);
        dparam1.promoteType(NLS_DOUBLE);
        retval << validateRealColor<double>(NLS_DOUBLE, dparam1, isMultiple);
    } break;
    case NLS_CELL_ARRAY: {
        if (!param1.isCellArrayOfCharacterVectors()) {
            Error(L"Nelson:graphics:validatecolor:InvalidColorCell",
                L"RGB triplet, a character vector, or a string scalar, cell of row characters "
                L"expected.");
        }
        retval << validateTextColors(param1, isMultiple);
    } break;
    case NLS_STRING_ARRAY: {
        retval << validateTextColors(param1, isMultiple);
    } break;
    case NLS_CHAR: {
        std::wstring colorStr = param1.asWideString();
        std::vector<double> rgb;
        if (!ParseColorToRGB(colorStr, false, rgb)) {
            Error(L"Nelson:graphics:validatecolor:InvalidColor", L"Valid color expected.");
        } else {
            ArrayOf res = ArrayOf::doubleRowVector(3);
            double* dp = (double*)(res.dataPointer());
            if (dp) {
                dp[0] = rgb[0];
                dp[1] = rgb[1];
                dp[2] = rgb[2];
            }
            retval << res;
        }
    } break;
    default: {
        Error(L"Nelson:graphics:validatecolor:InvalidColorType",
            L"Mx3 matrix of RGB triplets, a character vector, a cell-array of character vectors or "
            L"a string array expected.");
    } break;
    }
    return retval;
}
//=============================================================================
ArrayOf
validateTextColors(const ArrayOf& param1, bool isMultiple)
{
    indexType nElements = param1.elementCount();
    if (!isMultiple && nElements > 1) {
        Error(L"Nelson:graphics:validatecolor:MultipleColors",
            L"One color specification or use the 'multiple' option expected.");
    }
    if (nElements > 1 && !param1.isVector()) {
        Error(L"Nelson:graphics:validatecolor:InvalidStringShape",
            L"Specify colors using a 1-D cell-array of character vectors or a 1-D string array.");
    }
    Dimensions dimsC(nElements, 3);
    double* dptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.elementCount()));
    ArrayOf res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
    wstringVector elements = param1.asWideStringVector();
    for (indexType k = 0; k < nElements; ++k) {
        std::vector<double> rgb;
        if (!ParseColorToRGB(elements[k], false, rgb)) {
            Error(L"Nelson:graphics:validatecolor:InvalidColor", L"Valid color expected.");
        }
        dptr[k] = rgb[0];
        dptr[k + nElements] = rgb[1];
        dptr[k + (nElements * 2)] = rgb[2];
    }
    return res;
}
//=============================================================================
