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
#include "UminusSingle.hpp"
#include <Eigen/Dense>
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
single_uminus(ArrayOf a)
{
    Dimensions dim = a.getDimensions();
    indexType reslen = dim.getElementCount();
    size_t ma = dim.getRows();
    size_t na = dim.getColumns();
    void* pRes = nullptr;
    if (a.isComplex()) {
        pRes = new_with_exception<float>(2 * reslen);
        singlecomplex* Az = reinterpret_cast<singlecomplex*>((float*)a.getDataPointer());
        Eigen::Map<Eigen::MatrixXcf> matA(Az, ma, na);
        singlecomplex* pResz = reinterpret_cast<singlecomplex*>(pRes);
        Eigen::Map<Eigen::MatrixXcf> matRes(pResz, ma, na);
        matRes = 0.0 - matA.array();
    } else {
        pRes = new_with_exception<float>(reslen);
        Eigen::Map<Eigen::MatrixXf> matA((float*)a.getDataPointer(), ma, na);
        Eigen::Map<Eigen::MatrixXf> matRes((float*)pRes, ma, na);
        matRes = 0.0 - matA.array();
    }
    return ArrayOf(a.getDataClass(), dim, pRes, false);
}
//=============================================================================
}
//=============================================================================
