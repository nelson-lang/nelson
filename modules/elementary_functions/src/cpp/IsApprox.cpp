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
#include <boost/container/vector.hpp>
#include "IsApprox.hpp"
#include "RealPart.hpp"
#include "ImagPart.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
boost::container::vector<indexType>
nanIndices(T* values, indexType nbElements)
{
    boost::container::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isnan(values[k])) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
boost::container::vector<indexType>
infIndices(T* values, indexType nbElements)
{
    boost::container::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isinf(values[k]) && (values[k] > 0)) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
boost::container::vector<indexType>
ninfIndices(T* values, indexType nbElements)
{
    boost::container::vector<indexType> res;
    for (indexType k = 0; k < nbElements; k++) {
        if (std::isinf(values[k]) && (values[k] < 0)) {
            res.push_back(k);
        }
    }
    return res;
}
//=============================================================================
template <class T>
bool
IsApprox(T* ptrA, T* ptrB, const Dimensions& dims, double precision)
{
    bool bRes = false;
    boost::container::vector<indexType> infA = infIndices<T>(ptrA, dims.getElementCount());
    boost::container::vector<indexType> infB = infIndices<T>(ptrB, dims.getElementCount());
    if (infA.size() != infB.size()) {
        return false;
    }
    for (indexType k = 0; k < infA.size(); k++) {
        if (infA[k] != infB[k]) {
            return false;
        } else {
            ptrA[infA[k]] = 0;
            ptrB[infB[k]] = 0;
        }
    }
    infA.clear();
    infB.clear();
    boost::container::vector<indexType> ninfA = ninfIndices<T>(ptrA, dims.getElementCount());
    boost::container::vector<indexType> ninfB = ninfIndices<T>(ptrB, dims.getElementCount());
    if (ninfA.size() != ninfB.size()) {
        return false;
    }
    for (indexType k = 0; k < ninfA.size(); k++) {
        if (ninfA[k] != ninfB[k]) {
            return false;
        } else {
            ptrA[ninfA[k]] = 0;
            ptrB[ninfB[k]] = 0;
        }
    }
    ninfA.clear();
    ninfB.clear();
    boost::container::vector<indexType> nanA = nanIndices<T>(ptrA, dims.getElementCount());
    boost::container::vector<indexType> nanB = nanIndices<T>(ptrB, dims.getElementCount());
    if (nanA.size() != nanB.size()) {
        return false;
    }
    for (indexType k = 0; k < nanA.size(); k++) {
        if (nanA[k] != nanB[k]) {
            return false;
        } else {
            ptrA[nanA[k]] = 0;
            ptrB[nanB[k]] = 0;
        }
    }
    nanA.clear();
    nanB.clear();
    if (!dims.is2D()) {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, 1, dims.getElementCount());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, 1, dims.getElementCount());
        bRes = matA.isApprox(matB, (T)precision);
    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, dims.getRows(), dims.getColumns());
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, dims.getRows(), dims.getColumns());
        bRes = matA.isApprox(matB, (T)precision);
    }
    return bRes;
}
//=============================================================================
bool
IsApprox(ArrayOf A, ArrayOf B, double precision)
{
    bool bRes = false;
    if (A.getDataClass() == B.getDataClass()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (A.isEmpty() || B.isEmpty()) {
            return dimsA.equals(dimsB);
        }
        if (!dimsA.equals(dimsB)) {
            return false;
        }
        if (A.getDataClass() == NLS_SINGLE) {
            single* ptrA = (single*)A.getReadWriteDataPointer();
            single* ptrB = (single*)B.getReadWriteDataPointer();
            bRes = IsApprox<single>(ptrA, ptrB, dimsA, precision);
        } else if (A.getDataClass() == NLS_DOUBLE) {
            double* ptrA = (double*)A.getReadWriteDataPointer();
            double* ptrB = (double*)B.getReadWriteDataPointer();
            bRes = IsApprox<double>(ptrA, ptrB, dimsA, precision);
        } else if ((A.getDataClass() == NLS_DCOMPLEX) || (A.getDataClass() == NLS_SCOMPLEX)) {
            ArrayOf realpartA = RealPart(A);
            ArrayOf realpartB = RealPart(B);
            if (IsApprox(realpartA, realpartB, precision)) {
                ArrayOf imagpartA = ImagPart(A);
                ArrayOf imagpartB = ImagPart(B);
                bRes = IsApprox(imagpartA, imagpartB, precision);
            }
        }
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
