//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <vector>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "splineBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ArrayOf
rowVectorFromStd(const std::vector<double>& values)
{
    double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions(1, static_cast<indexType>(values.size())), ptr, false);
}
//=============================================================================
std::vector<double>
asRealVector(const ArrayOf& arg, const std::wstring& name)
{
    if (!arg.isNumeric() || arg.isComplex() || !arg.isVector()) {
        Error(
            L"Nelson:types:realNumericVectorExpected", L"%s must be a real numeric vector.", name);
    }
    ArrayOf value(arg);
    value.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(value.dataPointer());
    return std::vector<double>(ptr, ptr + value.elementCount());
}
//=============================================================================
std::vector<double>
splineCoefs(const std::vector<double>& x, const std::vector<double>& y)
{
    indexType n = static_cast<indexType>(x.size());
    std::vector<double> coefs(static_cast<size_t>((n - 1) * 4), 0.);
    if (n == 2) {
        coefs[2] = (y[1] - y[0]) / (x[1] - x[0]);
        coefs[3] = y[0];
        return coefs;
    }
    if (n == 3) {
        Eigen::Matrix3d V;
        Eigen::Vector3d rhs;
        for (indexType k = 0; k < 3; ++k) {
            V(k, 0) = x[k] * x[k];
            V(k, 1) = x[k];
            V(k, 2) = 1.;
            rhs(k) = y[k];
        }
        Eigen::Vector3d p = V.colPivHouseholderQr().solve(rhs);
        for (indexType k = 0; k < 2; ++k) {
            coefs[static_cast<size_t>(k * 4 + 1)] = p(0);
            coefs[static_cast<size_t>(k * 4 + 2)] = 2. * p(0) * x[k] + p(1);
            coefs[static_cast<size_t>(k * 4 + 3)] = p(0) * x[k] * x[k] + p(1) * x[k] + p(2);
        }
        return coefs;
    }
    std::vector<double> h(static_cast<size_t>(n - 1));
    std::vector<double> delta(static_cast<size_t>(n - 1));
    for (indexType k = 0; k < n - 1; ++k) {
        h[static_cast<size_t>(k)] = x[static_cast<size_t>(k + 1)] - x[static_cast<size_t>(k)];
        delta[static_cast<size_t>(k)] = (y[static_cast<size_t>(k + 1)] - y[static_cast<size_t>(k)])
            / h[static_cast<size_t>(k)];
    }
    // The second-derivative system is banded (tridiagonal except for the two
    // not-a-knot boundary rows), so it is built sparse and solved with a sparse
    // LU in O(n).
    std::vector<Eigen::Triplet<double>> triplets;
    triplets.reserve(static_cast<size_t>(3 * n));
    Eigen::VectorXd rhs = Eigen::VectorXd::Zero(n);
    triplets.emplace_back(0, 0, h[1]);
    triplets.emplace_back(0, 1, -(h[0] + h[1]));
    triplets.emplace_back(0, 2, h[0]);
    for (indexType k = 1; k < n - 1; ++k) {
        triplets.emplace_back((int)k, (int)(k - 1), h[static_cast<size_t>(k - 1)]);
        triplets.emplace_back(
            (int)k, (int)k, 2. * (h[static_cast<size_t>(k - 1)] + h[static_cast<size_t>(k)]));
        triplets.emplace_back((int)k, (int)(k + 1), h[static_cast<size_t>(k)]);
        rhs(k) = 6. * (delta[static_cast<size_t>(k)] - delta[static_cast<size_t>(k - 1)]);
    }
    triplets.emplace_back((int)(n - 1), (int)(n - 3), h[static_cast<size_t>(n - 2)]);
    triplets.emplace_back((int)(n - 1), (int)(n - 2),
        -(h[static_cast<size_t>(n - 3)] + h[static_cast<size_t>(n - 2)]));
    triplets.emplace_back((int)(n - 1), (int)(n - 1), h[static_cast<size_t>(n - 3)]);
    Eigen::SparseMatrix<double> A(n, n);
    A.setFromTriplets(triplets.begin(), triplets.end());
    A.makeCompressed();
    Eigen::SparseLU<Eigen::SparseMatrix<double>> solver;
    solver.analyzePattern(A);
    solver.factorize(A);
    Eigen::VectorXd m = solver.solve(rhs);
    for (indexType k = 0; k < n - 1; ++k) {
        double hk = h[static_cast<size_t>(k)];
        coefs[static_cast<size_t>(k * 4)] = (m(k + 1) - m(k)) / (6. * hk);
        coefs[static_cast<size_t>(k * 4 + 1)] = m(k) / 2.;
        coefs[static_cast<size_t>(k * 4 + 2)]
            = delta[static_cast<size_t>(k)] - hk * (2. * m(k) + m(k + 1)) / 6.;
        coefs[static_cast<size_t>(k * 4 + 3)] = y[static_cast<size_t>(k)];
    }
    return coefs;
}
//=============================================================================
ArrayOf
coefsArray(const std::vector<double>& coefs, indexType pieces)
{
    double* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, pieces * 4));
    for (indexType r = 0; r < pieces; ++r) {
        for (indexType c = 0; c < 4; ++c) {
            ptr[r + c * pieces] = coefs[static_cast<size_t>(r * 4 + c)];
        }
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(pieces, 4), ptr, false);
}
//=============================================================================
ArrayOf
ppStruct(const std::vector<double>& x, const std::vector<double>& coefs)
{
    indexType pieces = static_cast<indexType>(x.size() - 1);
    stringVector names { "form", "breaks", "coefs", "pieces", "order", "dim" };
    ArrayOfVector values;
    values << ArrayOf::characterArray("pp");
    values << rowVectorFromStd(x);
    values << coefsArray(coefs, pieces);
    values << ArrayOf::doubleScalar(static_cast<double>(pieces));
    values << ArrayOf::doubleScalar(4.);
    values << ArrayOf::doubleScalar(1.);
    return ArrayOf::structScalar(names, values);
}
//=============================================================================
ArrayOf
evaluateSpline(const std::vector<double>& x, const std::vector<double>& coefs, const ArrayOf& xq)
{
    ArrayOf q(xq);
    if (!q.isNumeric() || q.isComplex()) {
        Error(L"Nelson:types:queryPointsRealNumeric", L"Query points must be real numeric.");
    }
    q.promoteType(NLS_DOUBLE);
    Dimensions qDims = q.dimensions();
    indexType nQuery = q.elementCount();
    const double* qPtr = static_cast<const double*>(q.dataPointer());
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nQuery));
    indexType pieces = static_cast<indexType>(x.size() - 1);
    for (indexType k = 0; k < nQuery; ++k) {
        double query = qPtr[k];
        indexType piece = 0;
        if (query >= x.back()) {
            piece = pieces - 1;
        } else if (query > x.front()) {
            auto it = std::upper_bound(x.begin(), x.end(), query);
            piece = static_cast<indexType>(std::distance(x.begin(), it) - 1);
            if (piece >= pieces) {
                piece = pieces - 1;
            }
        }
        double dx = query - x[static_cast<size_t>(piece)];
        const double* c = coefs.data() + piece * 4;
        out[k] = ((c[0] * dx + c[1]) * dx + c[2]) * dx + c[3];
    }
    return ArrayOf(NLS_DOUBLE, qDims, out, false);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::splineBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    std::vector<double> x = asRealVector(argIn[0], L"X");
    std::vector<double> y = asRealVector(argIn[1], L"Y");
    if (x.size() != y.size()) {
        Error(L"Nelson:dimagree:xYLengthX", L"X and Y must have the same length.");
    }
    if (x.size() < 2) {
        Error(L"Nelson:special_functions:notEnoughSamplePoints",
            L"At least two sample points expected.");
    }
    for (size_t k = 1; k < x.size(); ++k) {
        if (!(x[k] > x[k - 1])) {
            Error(L"Nelson:interp:samplePointsNotIncreasing",
                L"Sample points must be strictly increasing.");
        }
    }
    std::vector<double> coefs = splineCoefs(x, y);
    ArrayOfVector retval;
    if (argIn.size() == 3) {
        retval << evaluateSpline(x, coefs, argIn[2]);
    } else {
        retval << ppStruct(x, coefs);
    }
    return retval;
}
//=============================================================================
