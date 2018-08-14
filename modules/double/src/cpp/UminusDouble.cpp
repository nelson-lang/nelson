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
#include "UminusDouble.hpp"
#include <Eigen/Dense>
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
double_uminus(ArrayOf a)
{
    Dimensions dim = a.getDimensions();
    indexType reslen = dim.getElementCount();
    size_t ma = dim.getRows();
    size_t na = dim.getColumns();
    void* pRes = nullptr;
    if (a.isComplex()) {
        pRes = new_with_exception<double>(2 * reslen);
        doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, ma, na);
        doublecomplex* pResz = reinterpret_cast<doublecomplex*>(pRes);
        Eigen::Map<Eigen::MatrixXcd> matRes(pResz, ma, na);
        matRes = 0.0 - matA.array();
    } else {
        pRes = new_with_exception<double>(reslen);
        Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), ma, na);
        Eigen::Map<Eigen::MatrixXd> matRes((double*)pRes, ma, na);
        matRes = 0.0 - matA.array();
    }
    return ArrayOf(a.getDataClass(), dim, pRes, false);
}
//=============================================================================
}
//=============================================================================
