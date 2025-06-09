//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "validatecolorBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOColorHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
ArrayOf
validateRealColor(NelsonType nlsType, const ArrayOf param1, bool isMultiple)
{
    ArrayOf res;
    if (param1.getColumns() != 3 || param1.isEmpty()) {
        Error(_W("RGB triplet expected."), L"Nelson:graphics:validatecolor:InvalidColor");
    }
    if (param1.getRows() > 1 && !isMultiple) {
        Error(_W("One color specification or use the 'multiple' option expected."),
            L"Nelson:graphics:validatecolor:MultipleColors");
    }
    if (param1.getRows() == 1) {
        Dimensions dimsC(param1.getRows(), 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount()));
        res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        std::vector<double> rgb;
        if (!ParseColorToRGB(param1, rgb)) {
            Error(_W("Valid color expected."), L"Nelson:graphics:validatecolor:InvalidColor");
        }
        memcpy(dptr, rgb.data(), 3 * sizeof(double));
    } else {
        Dimensions dimsC(param1.getRows(), 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount()));
        res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        const T* values = static_cast<const T*>(param1.getDataPointer());
        for (indexType k = 0; k < param1.getRows(); ++k) {
            Dimensions dimsArg(1, 3);
            T* uptr = static_cast<T*>(ArrayOf::allocateArrayOf(nlsType, 3));
            ArrayOf arg = ArrayOf(nlsType, dimsArg, uptr);
            uptr[0] = values[k];
            uptr[1] = values[k + (param1.getRows())];
            uptr[2] = values[k + (2 * param1.getRows())];
            std::vector<double> rgb;
            if (!ParseColorToRGB(arg, rgb)) {
                Error(_W("Valid color expected."), L"Nelson:graphics:validatecolor:InvalidColor");
            }
            dptr[k] = rgb[0];
            dptr[k + (param1.getRows())] = rgb[1];
            dptr[k + (2 * param1.getRows())] = rgb[2];
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
        std::wstring param2 = argIn[1].getContentAsWideString();
        if (param2 == L"one") {
            isMultiple = false;
        } else if (param2 == L"multiple") {
            isMultiple = true;
        } else {
            Error(_("Invalid #2 argument: 'one', 'multiple' expected."));
        }
    }
    ArrayOf param1 = argIn[0];
    switch (param1.getDataClass()) {
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
            Error(_W("RGB triplet, a character vector, or a string scalar, cell of row characters "
                     "expected."),
                L"Nelson:graphics:validatecolor:InvalidColor");
        }
        indexType nElements = param1.getElementCount();
        if (!isMultiple && nElements > 1) {
            Error(_W("Use the 'multiple' option or specify one color specification."),
                L"Nelson:graphics:validatecolor:MultipleColors");
        }
        wstringVector elements = param1.getContentAsWideStringVector();
        Dimensions dimsC(nElements, 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount()));
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        for (indexType k = 0; k < nElements; ++k) {
            std::vector<double> rgb;
            if (!ParseColorToRGB(elements[k], false, rgb)) {
                Error(_W("Valid color expected."), L"Nelson:graphics:validatecolor:InvalidColor");
            }
            dptr[k] = rgb[0];
            dptr[k + nElements] = rgb[1];
            dptr[k + (nElements * 2)] = rgb[2];
        }
        retval << res;
    } break;
    case NLS_STRING_ARRAY: {
        indexType nElements = param1.getElementCount();
        if (!isMultiple && nElements > 1) {
            Error(_W("Use the 'multiple' option or specify one color specification."),
                L"Nelson:graphics:validatecolor:MultipleColors");
        }
        wstringVector elements = param1.getContentAsWideStringVector();
        Dimensions dimsC(nElements, 3);
        double* dptr
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsC.getElementCount()));
        ArrayOf res = ArrayOf(NLS_DOUBLE, dimsC, dptr);
        for (indexType k = 0; k < nElements; ++k) {
            std::vector<double> rgb;
            if (!ParseColorToRGB(elements[k], false, rgb)) {
                Error(_W("Valid color expected."), L"Nelson:graphics:validatecolor:InvalidColor");
            }
            dptr[k] = rgb[0];
            dptr[k + nElements] = rgb[1];
            dptr[k + (nElements * 2)] = rgb[2];
        }
        retval << res;
    } break;
    case NLS_CHAR: {
        std::wstring colorStr = param1.getContentAsWideString();
        std::vector<double> rgb;
        if (!ParseColorToRGB(colorStr, false, rgb)) {
            Error(
                _W("Valid color string expected."), L"Nelson:graphics:validatecolor:InvalidColor");
        } else {
            ArrayOf res = ArrayOf::doubleRowVectorConstructor(3);
            double* dp = (double*)(res.getDataPointer());
            if (dp) {
                dp[0] = rgb[0];
                dp[1] = rgb[1];
                dp[2] = rgb[2];
            }
            retval << res;
        }
    } break;
    default: {
        Error(_W("Mx3 matrix of RGB triplets, a character vector, a cell-array of character "
                 "vectors or a string array expected."),
            L"Nelson:graphics:validatecolor:InvalidColor");
    } break;
    }
    return retval;
}
//=============================================================================
