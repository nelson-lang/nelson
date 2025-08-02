//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "TraceMatrix.hpp"
#include "ClassName.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
TraceMatrix(const ArrayOf& A)
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
            Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getRows(),
                (Eigen::Index)A.getColumns());
            double res = matA.trace();
            return ArrayOf::doubleConstructor(res);
        } // NLS_DCOMPLEX

        auto* Az = reinterpret_cast<doublecomplex*>((single*)A.getDataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(
            Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        doublecomplex res = matA.trace();
        return ArrayOf::dcomplexConstructor(res.real(), res.imag());
    }
    if (A.getDataClass() == NLS_SINGLE) {
        Eigen::Map<Eigen::MatrixXf> matA(
            (single*)A.getDataPointer(), (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
        return ArrayOf::singleConstructor(matA.trace());
    } // NLS_SCOMPLEX
    auto* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    singlecomplex res = matA.trace();
    return ArrayOf::complexConstructor(res.real(), res.imag());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
