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
#include "AbsoluteValue.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
AbsoluteValue(const ArrayOf& arrayIn, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (arrayIn.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    switch (arrayIn.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        res = arrayIn;
        res.ensureSingleOwner();
    } break;
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = arrayIn;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
    } break;
    case NLS_INT8: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (int8*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<int8, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (int8*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_INT16: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (int16*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<int16, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (int16*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_INT32: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (int32*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<int32, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (int32*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_INT64: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (int64*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (int64*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_SINGLE: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (single*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<single, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (single*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_DOUBLE: {
        res = arrayIn;
        res.ensureSingleOwner();
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matOrigin(
            (double*)arrayIn.getDataPointer(), 1, dimsArrayIn.getElementCount());
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>> matAbs(
            (double*)res.getDataPointer(), 1, dimsArrayIn.getElementCount());
        matAbs = matOrigin.cwiseAbs();
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        double* dp = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, dimsArrayIn.getElementCount(), stringVector(), false);
        doublecomplex* matzArrayIn
            = reinterpret_cast<doublecomplex*>((double*)arrayIn.getDataPointer());
        for (indexType k = 0; k < dimsArrayIn.getElementCount(); ++k) {
            dp[k] = std::sqrt((matzArrayIn[k].real() * matzArrayIn[k].real())
                + (matzArrayIn[k].imag() * matzArrayIn[k].imag()));
        }
        res = ArrayOf(NLS_DOUBLE, dimsArrayIn, dp);
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsArrayIn = arrayIn.getDimensions();
        single* dp = (single*)ArrayOf::allocateArrayOf(
            NLS_SINGLE, dimsArrayIn.getElementCount(), stringVector(), false);
        singlecomplex* matzArrayIn
            = reinterpret_cast<singlecomplex*>((single*)arrayIn.getDataPointer());
        for (indexType k = 0; k < dimsArrayIn.getElementCount(); ++k) {
            dp[k] = std::sqrt((matzArrayIn[k].real() * matzArrayIn[k].real())
                + (matzArrayIn[k].imag() * matzArrayIn[k].imag()));
        }
        res = ArrayOf(NLS_SINGLE, dimsArrayIn, dp);
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
