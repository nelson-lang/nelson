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
#include <Eigen/Dense>
#include <cmath>
#include <functional>
#include "TruncateFunctions.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
typedef enum
{
    ROUND,
    CEIL,
    FIX,
    FLOOR
} TRUNCATE_LEVEL;
//=============================================================================
static double
fixDouble(double a)
{
    return std::trunc(a);
}
//=============================================================================
static single
fixSingle(single a)
{
    return std::trunc(a);
}
//=============================================================================
static double
ceilDouble(double a)
{
    return ceil(a);
}
//=============================================================================
static single
ceilSingle(single a)
{
    return ceil(a);
}
//=============================================================================
static double
roundDouble(double a)
{
    return round(a);
}
//=============================================================================
static single
roundSingle(single a)
{
    return round(a);
}
//=============================================================================
static double
floorDouble(double a)
{
    return floor(a);
}
//=============================================================================
static single
floorSingle(single a)
{
    return round(a);
}
//=============================================================================
template <class T>
ArrayOf
truncateArray(ArrayOf arrayIn, T (*ptrFunc)(T))
{
    size_t len = arrayIn.getLength();
    void* ptr = ArrayOf::allocateArrayOf(arrayIn.getDataClass(), len);
    T* rp = (T*)ptr;
    T* dp = (T*)arrayIn.getDataPointer();
    for (size_t i = 0; i < len * 2; i++) {
        if (std::isfinite(dp[i])) {
            rp[i] = (ptrFunc)((T)dp[i]);
        } else {
            rp[i] = dp[i];
        }
    }
    return ArrayOf(arrayIn.getDataClass(), arrayIn.getDimensions(), rp);
}
//=============================================================================
static ArrayOf
Truncate(ArrayOf arrayIn, TRUNCATE_LEVEL level)
{
    ArrayOf res;
    if (arrayIn.isSparse()) {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"ceil"
                + L"'");
            break;
        case TRUNCATE_LEVEL::ROUND:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"round"
                + L"'");
            break;
        case TRUNCATE_LEVEL::FIX:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"fix"
                + L"'");
            break;
        case TRUNCATE_LEVEL::FLOOR:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"floor"
                + L"'");
            break;
        }
    }
    switch (arrayIn.getDataClass()) {
    case NLS_SCOMPLEX: {
        single (*ptr)(single);
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            ptr = &(ceilSingle);
            return truncateArray<single>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            ptr = &(roundSingle);
            return truncateArray<single>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            ptr = &(fixSingle);
            return truncateArray<single>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            ptr = &(floorSingle);
            return truncateArray<single>(arrayIn, ptr);
        } break;
        }
        return res;
    } break;
    case NLS_DCOMPLEX: {
        double (*ptr)(double);
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            ptr = &(ceilDouble);
            return truncateArray<double>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            ptr = &(roundDouble);
            return truncateArray<double>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            ptr = &(fixDouble);
            return truncateArray<double>(arrayIn, ptr);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            ptr = &(floorDouble);
            return truncateArray<double>(arrayIn, ptr);
        } break;
        }
    } break;
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"ceil"
                + L"'");
            break;
        case TRUNCATE_LEVEL::ROUND:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"round"
                + L"'");
            break;
        case TRUNCATE_LEVEL::FIX:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"fix"
                + L"'");
            break;
        case TRUNCATE_LEVEL::FLOOR:
            Error(_W("Undefined function '") + utf8_to_wstring(ClassName(arrayIn)) + L"_" + L"floor"
                + L"'");
            break;
        }
    } break;
    case NLS_CHAR: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
        double* rp = (double*)ptr;
        charType* dp = (charType*)arrayIn.getDataPointer();
        for (size_t i = 0; i < len; i++) {
            rp[i] = (double)dp[i];
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_LOGICAL: {
        size_t len = arrayIn.getLength();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len);
        double* rp = (double*)ptr;
        logical* dp = (logical*)arrayIn.getDataPointer();
        for (size_t i = 0; i < len; i++) {
            rp[i] = (dp[i] == 0 ? 0 : 1);
        }
        res = ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_DOUBLE:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE: {
        ArrayOf InputAsDouble(arrayIn);
        InputAsDouble.ensureSingleOwner();
        InputAsDouble.promoteType(NLS_DOUBLE);
        ArrayOf OutputAsDouble(arrayIn);
        OutputAsDouble.ensureSingleOwner();
        OutputAsDouble.promoteType(NLS_DOUBLE);
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            // to speed up computations, we use a vector with eigen library and MKL
            Eigen::Map<Eigen::MatrixXd> matA(
                (double*)InputAsDouble.getDataPointer(), 1, InputAsDouble.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(
                (double*)OutputAsDouble.getDataPointer(), 1, OutputAsDouble.getLength());
            matR = matA.array().ceil();
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            // to speed up computations, we use a vector with eigen library and MKL
            Eigen::Map<Eigen::MatrixXd> matA(
                (double*)InputAsDouble.getDataPointer(), 1, InputAsDouble.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(
                (double*)OutputAsDouble.getDataPointer(), 1, OutputAsDouble.getLength());
            matR = matA.array().round();

        } break;
        case TRUNCATE_LEVEL::FIX: {
            Eigen::Map<Eigen::MatrixXd> matA(
                (double*)InputAsDouble.getDataPointer(), 1, InputAsDouble.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(
                (double*)OutputAsDouble.getDataPointer(), 1, OutputAsDouble.getLength());
            matR = matA.unaryExpr(std::ref(fixDouble));

        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            // to speed up computations, we use a vector with eigen library and MKL
            Eigen::Map<Eigen::MatrixXd> matA(
                (double*)InputAsDouble.getDataPointer(), 1, InputAsDouble.getLength());
            Eigen::Map<Eigen::MatrixXd> matR(
                (double*)OutputAsDouble.getDataPointer(), 1, OutputAsDouble.getLength());
            matR = matA.array().floor();
        } break;
        }
        OutputAsDouble.promoteType(arrayIn.getDataClass());
        return OutputAsDouble;
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Round(ArrayOf arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::ROUND);
}
//=============================================================================
ArrayOf
Ceil(ArrayOf arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::CEIL);
}
//=============================================================================
ArrayOf
Floor(ArrayOf arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FLOOR);
}
//=============================================================================
ArrayOf
Fix(ArrayOf arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FIX);
}
//=============================================================================
}
//=============================================================================
