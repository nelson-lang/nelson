//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <complex>
#include <limits>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "PageInverse.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
invPageD(double* page, int n)
{
    std::vector<int> ipiv(n);
    int info = 0;
    int nn = n;
    LAPACK_dgetrf(&nn, &nn, page, &nn, ipiv.data(), &info);
    if (info == 0) {
        int lwork = std::max(1, 4 * n);
        std::vector<double> work(lwork);
        LAPACK_dgetri(&nn, page, &nn, ipiv.data(), work.data(), &lwork, &info);
    }
    if (info != 0) {
        double inf = std::numeric_limits<double>::infinity();
        for (int i = 0; i < n * n; ++i) {
            page[i] = inf;
        }
    }
}
//=============================================================================
static void
invPageS(single* page, int n)
{
    std::vector<int> ipiv(n);
    int info = 0;
    int nn = n;
    LAPACK_sgetrf(&nn, &nn, page, &nn, ipiv.data(), &info);
    if (info == 0) {
        int lwork = std::max(1, 4 * n);
        std::vector<single> work(lwork);
        LAPACK_sgetri(&nn, page, &nn, ipiv.data(), work.data(), &lwork, &info);
    }
    if (info != 0) {
        single inf = std::numeric_limits<single>::infinity();
        for (int i = 0; i < n * n; ++i) {
            page[i] = inf;
        }
    }
}
//=============================================================================
static void
invPageZ(doublecomplex* page, int n)
{
    std::vector<int> ipiv(n);
    int info = 0;
    int nn = n;
    LAPACK_zgetrf(&nn, &nn, page, &nn, ipiv.data(), &info);
    if (info == 0) {
        int lwork = std::max(1, 4 * n);
        std::vector<doublecomplex> work(lwork);
        LAPACK_zgetri(&nn, page, &nn, ipiv.data(), work.data(), &lwork, &info);
    }
    if (info != 0) {
        doublecomplex inf(std::numeric_limits<double>::infinity(), 0.0);
        for (int i = 0; i < n * n; ++i) {
            page[i] = inf;
        }
    }
}
//=============================================================================
static void
invPageC(singlecomplex* page, int n)
{
    std::vector<int> ipiv(n);
    int info = 0;
    int nn = n;
    LAPACK_cgetrf(&nn, &nn, page, &nn, ipiv.data(), &info);
    if (info == 0) {
        int lwork = std::max(1, 4 * n);
        std::vector<singlecomplex> work(lwork);
        LAPACK_cgetri(&nn, page, &nn, ipiv.data(), work.data(), &lwork, &info);
    }
    if (info != 0) {
        singlecomplex inf(std::numeric_limits<single>::infinity(), 0.0f);
        for (int i = 0; i < n * n; ++i) {
            page[i] = inf;
        }
    }
}
//=============================================================================
ArrayOf
PageInverse(const ArrayOf& X, bool& needToOverload)
{
    needToOverload = false;
    NelsonType cl = X.dataClass();
    bool supported
        = (cl == NLS_DOUBLE || cl == NLS_SINGLE || cl == NLS_DCOMPLEX || cl == NLS_SCOMPLEX)
        && !X.isSparse();
    if (!supported) {
        needToOverload = true;
        return {};
    }
    Dimensions dims = X.dimensions();
    indexType r = dims.at(0, false);
    indexType c = dims.at(1, false);
    if (r != c) {
        Error("Nelson:linear_algebra:eachPageSquareMatrixExpected",
            "Each page of the input array must be a square matrix.");
    }
    ArrayOf res(X);
    res.ensureSingleOwner();
    if (X.isEmpty()) {
        return res;
    }
    int n = (int)r;
    indexType pages = 1;
    for (indexType i = 2; i < (indexType)dims.length(); ++i) {
        pages *= dims.at(i, false);
    }
    indexType pageElems = r * c;

    switch (cl) {
    case NLS_DOUBLE: {
        auto* data = (double*)res.dataPointer();
        OMP_PARALLEL_FOR_LOOP(pages)
        for (ompIndexType p = 0; p < (ompIndexType)pages; ++p) {
            invPageD(data + (indexType)p * pageElems, n);
        }
    } break;
    case NLS_SINGLE: {
        auto* data = (single*)res.dataPointer();
        OMP_PARALLEL_FOR_LOOP(pages)
        for (ompIndexType p = 0; p < (ompIndexType)pages; ++p) {
            invPageS(data + (indexType)p * pageElems, n);
        }
    } break;
    case NLS_DCOMPLEX: {
        auto* data = reinterpret_cast<doublecomplex*>((double*)res.dataPointer());
        OMP_PARALLEL_FOR_LOOP(pages)
        for (ompIndexType p = 0; p < (ompIndexType)pages; ++p) {
            invPageZ(data + (indexType)p * pageElems, n);
        }
    } break;
    case NLS_SCOMPLEX: {
        auto* data = reinterpret_cast<singlecomplex*>((single*)res.dataPointer());
        OMP_PARALLEL_FOR_LOOP(pages)
        for (ompIndexType p = 0; p < (ompIndexType)pages; ++p) {
            invPageC(data + (indexType)p * pageElems, n);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
