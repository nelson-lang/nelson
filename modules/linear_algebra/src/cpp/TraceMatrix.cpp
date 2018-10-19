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
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "TraceMatrix.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
TraceMatrix(ArrayOf A)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'trace' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (A.isEmpty()) {
        return ArrayOf::doubleConstructor(0);
    }
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            double res = matA.trace();
            return ArrayOf::doubleConstructor(res);
        } else // NLS_DCOMPLEX
        {
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((single*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            doublecomplex res = matA.trace();
            return ArrayOf::dcomplexConstructor(res.real(), res.imag());
        }
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(),
                (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            return ArrayOf::singleConstructor(matA.trace());
        } else // NLS_SCOMPLEX
        {
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(),
                (Eigen::Index)A.getDimensions().getColumns());
            singlecomplex res = matA.trace();
            return ArrayOf::complexConstructor(res.real(), res.imag());
        }
    }
}
//=============================================================================
}
//=============================================================================
