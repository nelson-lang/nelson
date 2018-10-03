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
#include <limits>
#include "Gamma.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double
gammad(double v)
{
    if ((v == 0) || std::isinf(v) || (v >= 172)) {
        return std::numeric_limits<double>::infinity();
    }
    if ((v <= 0) && (floor(v) == v)) {
        return std::numeric_limits<double>::infinity();
    }
    return std::tgamma(v);
}
//=============================================================================
static single
gammas(single v)
{
    if ((v == 0) || std::isinf(v) || (v >= 36)) {
        return std::numeric_limits<single>::infinity();
    }
    if ((v <= 0) && (floor(v) == v)) {
        return std::numeric_limits<single>::infinity();
    }
    return std::tgamma(v);
}
//=============================================================================
ArrayOf
Gamma(const ArrayOf& arrayIn)
{
    ArrayOf res;
    if (!arrayIn.isEmpty()) {
        Dimensions dimsIn = arrayIn.getDimensions();
        if (arrayIn.getDataClass() == NLS_DOUBLE) {
            double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsIn.getElementCount(), stringVector(), false);
            double* ptrIn = (double*)arrayIn.getDataPointer();
            Eigen::Map<Eigen::ArrayXd> matOut(ptrOut, dimsIn.getElementCount());
            Eigen::Map<Eigen::ArrayXd> matIn(ptrIn, dimsIn.getElementCount());
            matOut = matIn.unaryExpr(std::ref(gammad));
            res = ArrayOf(NLS_DOUBLE, dimsIn, ptrOut);
        } else {
            single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, dimsIn.getElementCount(), stringVector(), false);
            single* ptrIn = (single*)arrayIn.getDataPointer();
            Eigen::Map<Eigen::ArrayXf> matOut(ptrOut, dimsIn.getElementCount());
            Eigen::Map<Eigen::ArrayXf> matIn(ptrIn, dimsIn.getElementCount());
            matOut = matIn.unaryExpr(std::ref(gammas));
            res = ArrayOf(NLS_SINGLE, dimsIn, ptrOut);
        }
    } else {
        res = arrayIn;
        res.ensureSingleOwner();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
