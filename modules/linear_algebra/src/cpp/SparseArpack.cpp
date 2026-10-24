//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SparseArpack.hpp"
#include "SparseArpackFallback.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NewWithException.hpp"
#include "SparseType.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <Eigen/Sparse>
#include <Eigen/SparseLU>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <complex>
#include <limits>
#include <memory>
#include <numeric>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
using SparseDoubleMatrix = Eigen::SparseMatrix<double, 0, signedIndexType>;
using SparseComplexMatrix = Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>;
struct SparseEigenResult
{
    Eigen::MatrixXd vectors;
    std::vector<double> values;
};
//=============================================================================
struct SparseGeneralEigenResult
{
    Eigen::MatrixXcd vectors;
    std::vector<std::complex<double>> values;
};
//=============================================================================
struct SparseEigsRequest
{
    std::string which;
    bool shiftInvert;
    doublecomplex sigma;
};
//=============================================================================
static bool
isFiniteSparseDouble(const SparseDoubleMatrix& mat)
{
    for (int k = 0; k < mat.outerSize(); ++k) {
        for (SparseDoubleMatrix::InnerIterator it(mat, k); it; ++it) {
            if (!std::isfinite(it.value())) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static bool
isFiniteSparseComplex(const SparseComplexMatrix& mat)
{
    for (int k = 0; k < mat.outerSize(); ++k) {
        for (SparseComplexMatrix::InnerIterator it(mat, k); it; ++it) {
            if (!std::isfinite(it.value().real()) || !std::isfinite(it.value().imag())) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static bool
isSymmetricSparseDouble(const SparseDoubleMatrix& mat)
{
    SparseDoubleMatrix diff = mat - SparseDoubleMatrix(mat.transpose());
    for (int k = 0; k < diff.outerSize(); ++k) {
        for (SparseDoubleMatrix::InnerIterator it(diff, k); it; ++it) {
            if (std::abs(it.value()) > 1e-12) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
static indexType
getSparseArpackK(const ArrayOfVector& argIn, indexType limit)
{
    if (limit < 2) {
        Error(L"Nelson:eigs:matrixDimensionTooSmall",
            L"Input matrix dimension must be greater than one.");
    }
    indexType k = std::min<indexType>(6, limit - 1);
    if (argIn.size() > 1) {
        k = argIn[1].asScalarIndex(true);
    }
    if (k < 1 || k >= limit) {
        Error(L"Nelson:eigs:invalidK",
            L"K must be a positive integer smaller than the matrix dimension.");
    }
    return k;
}
//=============================================================================
static std::string
toLowerAscii(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return value;
}
//=============================================================================
static std::string
getSparseArpackWhich(const ArrayOfVector& argIn)
{
    if (argIn.size() < 3) {
        return "lm";
    }
    ArrayOf which(argIn[2]);
    if (!(which.isRowVectorCharacterArray() || which.isScalarStringArray())) {
        Error(L"Nelson:eigs:invalidWhich", L"WHICH must be a string.");
    }
    std::string value = toLowerAscii(which.asUtf8String());
    if (value == "lm" || value == "sm" || value == "lr" || value == "sr" || value == "li"
        || value == "si" || value == "la" || value == "sa") {
        return value;
    }
    Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.", L"WHICH value");
    return "lm";
}
//=============================================================================
static SparseEigsRequest
getSparseEigsRequest(const ArrayOfVector& argIn)
{
    SparseEigsRequest request { "lm", false, 0.0 };
    if (argIn.size() < 3) {
        return request;
    }
    ArrayOf third(argIn[2]);
    if (third.isRowVectorCharacterArray() || third.isScalarStringArray()) {
        request.which = getSparseArpackWhich(argIn);
        return request;
    }
    if (!third.isScalar()) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    doublecomplex sigma = third.isComplex() ? third.asDoubleComplexScalar()
                                            : doublecomplex(third.asDoubleScalar(), 0.0);
    if (!std::isfinite(sigma.real()) || !std::isfinite(sigma.imag())) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    request.shiftInvert = true;
    request.sigma = sigma;
    return request;
}
//=============================================================================
static std::string
getSparseArpackSingularWhich(const ArrayOfVector& argIn)
{
    if (argIn.size() < 3) {
        return "lm";
    }
    ArrayOf which(argIn[2]);
    if (!(which.isRowVectorCharacterArray() || which.isScalarStringArray())) {
        Error(L"Nelson:eigs:invalidWhich", L"WHICH must be a string.");
    }
    std::string value = toLowerAscii(which.asUtf8String());
    if (value == "lm" || value == "largest") {
        return "lm";
    }
    if (value == "sm" || value == "smallest") {
        return "sm";
    }
    Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.", L"WHICH value");
    return "lm";
}
//=============================================================================
static std::string
getSymmetricArpackWhich(const std::string& which)
{
    if (which == "sm") {
        return "SM";
    }
    if (which == "lr" || which == "la") {
        return "LA";
    }
    if (which == "sr" || which == "sa") {
        return "SA";
    }
    return "LM";
}
//=============================================================================
static std::string
getGeneralArpackWhich(const std::string& which)
{
    if (which == "sm") {
        return "SM";
    }
    if (which == "lr" || which == "la") {
        return "LR";
    }
    if (which == "sr" || which == "sa") {
        return "SR";
    }
    if (which == "li") {
        return "LI";
    }
    if (which == "si") {
        return "SI";
    }
    return "LM";
}
//=============================================================================
static bool
orderDoubleEigenvalue(double left, double right, const std::string& which)
{
    if (which == "sm") {
        return std::abs(left) < std::abs(right);
    }
    if (which == "lr" || which == "la") {
        return left > right;
    }
    if (which == "sr" || which == "sa") {
        return left < right;
    }
    double leftAbs = std::abs(left);
    double rightAbs = std::abs(right);
    if (leftAbs == rightAbs) {
        return left > right;
    }
    return leftAbs > rightAbs;
}
//=============================================================================
static bool
orderComplexEigenvalue(
    const std::complex<double>& left, const std::complex<double>& right, const std::string& which)
{
    auto cleanImaginaryPart = [](double value) { return std::abs(value) < 1e-14 ? 0.0 : value; };
    if (which == "sm") {
        return std::abs(left) < std::abs(right);
    }
    if (which == "lr" || which == "la") {
        return left.real() > right.real();
    }
    if (which == "sr" || which == "sa") {
        return left.real() < right.real();
    }
    if (which == "li") {
        double leftImag = cleanImaginaryPart(left.imag());
        double rightImag = cleanImaginaryPart(right.imag());
        double leftAbsImag = std::abs(leftImag);
        double rightAbsImag = std::abs(rightImag);
        if (leftAbsImag != rightAbsImag) {
            return leftAbsImag > rightAbsImag;
        }
        if (leftImag != rightImag) {
            return leftImag > rightImag;
        }
    }
    if (which == "si") {
        double leftImag = cleanImaginaryPart(left.imag());
        double rightImag = cleanImaginaryPart(right.imag());
        double leftAbsImag = std::abs(leftImag);
        double rightAbsImag = std::abs(rightImag);
        if (leftAbsImag != rightAbsImag) {
            return leftAbsImag < rightAbsImag;
        }
        if (leftImag != rightImag) {
            return leftImag < rightImag;
        }
    }
    double leftAbs = std::abs(left);
    double rightAbs = std::abs(right);
    if (leftAbs == rightAbs) {
        return left.real() > right.real();
    }
    return leftAbs > rightAbs;
}
//=============================================================================
#if WITH_ARPACK
using SparseLuSolver = Eigen::SparseLU<SparseDoubleMatrix, Eigen::COLAMDOrdering<signedIndexType>>;
using SparseComplexLuSolver
    = Eigen::SparseLU<SparseComplexMatrix, Eigen::COLAMDOrdering<signedIndexType>>;
//=============================================================================
static std::unique_ptr<SparseLuSolver>
factorizeShiftedSparseMatrix(const SparseDoubleMatrix& mat, double sigma)
{
    SparseDoubleMatrix shifted(mat);
    signedIndexType n = static_cast<signedIndexType>(mat.rows());
    for (signedIndexType k = 0; k < n; ++k) {
        shifted.coeffRef(k, k) -= sigma;
    }
    shifted.makeCompressed();
    std::unique_ptr<SparseLuSolver> solver(new SparseLuSolver);
    solver->compute(shifted);
    if (solver->info() != Eigen::Success) {
        Error(L"Nelson:eigs:linearSolverFailed", L"%s failed.", L"Sparse linear equation solver");
    }
    return solver;
}
//=============================================================================
static std::unique_ptr<SparseComplexLuSolver>
factorizeShiftedSparseComplexMatrix(const SparseComplexMatrix& mat, const doublecomplex& sigma)
{
    SparseComplexMatrix shifted(mat);
    signedIndexType n = static_cast<signedIndexType>(mat.rows());
    for (signedIndexType k = 0; k < n; ++k) {
        shifted.coeffRef(k, k) -= sigma;
    }
    shifted.makeCompressed();
    std::unique_ptr<SparseComplexLuSolver> solver(new SparseComplexLuSolver);
    solver->compute(shifted);
    if (solver->info() != Eigen::Success) {
        Error(L"Nelson:eigs:linearSolverFailed", L"%s failed.", L"Sparse linear equation solver");
    }
    return solver;
}
//=============================================================================
extern "C"
{
    void
    dsaupd_(int* ido, char* bmat, int* n, char* which, int* nev, double* tol, double* resid,
        int* ncv, double* v, int* ldv, int* iparam, int* ipntr, double* workd, double* workl,
        int* lworkl, int* info, size_t bmat_len, size_t which_len);
    void
    dseupd_(int* rvec, char* howmny, int* select, double* d, double* z, int* ldz, double* sigma,
        char* bmat, int* n, char* which, int* nev, double* tol, double* resid, int* ncv, double* v,
        int* ldv, int* iparam, int* ipntr, double* workd, double* workl, int* lworkl, int* info,
        size_t howmny_len, size_t bmat_len, size_t which_len);
    void
    dnaupd_(int* ido, char* bmat, int* n, char* which, int* nev, double* tol, double* resid,
        int* ncv, double* v, int* ldv, int* iparam, int* ipntr, double* workd, double* workl,
        int* lworkl, int* info, size_t bmat_len, size_t which_len);
    void
    dneupd_(int* rvec, char* howmny, int* select, double* dr, double* di, double* z, int* ldz,
        double* sigmar, double* sigmai, double* workev, char* bmat, int* n, char* which, int* nev,
        double* tol, double* resid, int* ncv, double* v, int* ldv, int* iparam, int* ipntr,
        double* workd, double* workl, int* lworkl, int* info, size_t howmny_len, size_t bmat_len,
        size_t which_len);
    void
    znaupd_(int* ido, char* bmat, int* n, char* which, int* nev, double* tol, doublecomplex* resid,
        int* ncv, doublecomplex* v, int* ldv, int* iparam, int* ipntr, doublecomplex* workd,
        doublecomplex* workl, int* lworkl, double* rwork, int* info, size_t bmat_len,
        size_t which_len);
    void
    zneupd_(int* rvec, char* howmny, int* select, doublecomplex* d, doublecomplex* z, int* ldz,
        doublecomplex* sigma, doublecomplex* workev, char* bmat, int* n, char* which, int* nev,
        double* tol, doublecomplex* resid, int* ncv, doublecomplex* v, int* ldv, int* iparam,
        int* ipntr, doublecomplex* workd, doublecomplex* workl, int* lworkl, double* rwork,
        int* info, size_t howmny_len, size_t bmat_len, size_t which_len);
}
//=============================================================================
static SparseEigenResult
sparseSymmetricEigs(const SparseDoubleMatrix& mat, indexType k, const SparseEigsRequest& request)
{
    int n = static_cast<int>(mat.rows());
    int nev = static_cast<int>(k);
    int ncv = std::min(n, std::max(20, std::max(nev + 2, 2 * nev + 1)));
    int ido = 0;
    int info = 0;
    int ldv = n;
    int lworkl = ncv * (ncv + 8);
    char bmat = 'I';
    std::string arpackWhich = request.shiftInvert ? "LM" : getSymmetricArpackWhich(request.which);
    std::unique_ptr<SparseLuSolver> shiftSolver;
    if (request.shiftInvert) {
        shiftSolver = factorizeShiftedSparseMatrix(mat, request.sigma.real());
    }
    double tol = 0.0;
    std::vector<double> resid(n, 0.0);
    std::vector<double> v(static_cast<size_t>(n) * ncv, 0.0);
    int iparam[11] = {};
    int ipntr[11] = {};
    std::vector<double> workd(static_cast<size_t>(3) * n, 0.0);
    std::vector<double> workl(lworkl, 0.0);
    iparam[0] = 1;
    iparam[2] = 300;
    iparam[6] = request.shiftInvert ? 3 : 1;
    do {
        dsaupd_(&ido, &bmat, &n, arpackWhich.data(), &nev, &tol, resid.data(), &ncv, v.data(), &ldv,
            iparam, ipntr, workd.data(), workl.data(), &lworkl, &info, 1, 2);
        if (ido == 1 || ido == -1) {
            Eigen::Map<const Eigen::VectorXd> x(&workd[ipntr[0] - 1], n);
            Eigen::Map<Eigen::VectorXd> y(&workd[ipntr[1] - 1], n);
            if (request.shiftInvert) {
                y = shiftSolver->solve(x);
                if (shiftSolver->info() != Eigen::Success) {
                    Error(L"Nelson:eigs:linearSolverFailed", L"%s failed.",
                        L"Sparse linear equation solver");
                }
            } else {
                y.noalias() = mat * x;
            }
        }
    } while (ido == 1 || ido == -1);
    if (info != 0) {
        Error(L"Nelson:eigs:solverFailed", L"%s failed.", L"ARPACK eigensolver");
    }
    int rvec = 1;
    char howmny = 'A';
    std::vector<int> select(ncv, 0);
    std::vector<double> values(nev, 0.0);
    std::vector<double> vectors(static_cast<size_t>(n) * nev, 0.0);
    double sigma = request.shiftInvert ? request.sigma.real() : 0.0;
    dseupd_(&rvec, &howmny, select.data(), values.data(), vectors.data(), &ldv, &sigma, &bmat, &n,
        arpackWhich.data(), &nev, &tol, resid.data(), &ncv, v.data(), &ldv, iparam, ipntr,
        workd.data(), workl.data(), &lworkl, &info, 1, 1, 2);
    if (info != 0) {
        Error(L"Nelson:eigs:eigenvectorExtractionFailed", L"%s failed.",
            L"ARPACK eigenvector extraction");
    }
    std::vector<size_t> order(values.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&values, &request](size_t a, size_t b) {
        if (request.shiftInvert) {
            double da = std::abs(values[a] - request.sigma.real());
            double db = std::abs(values[b] - request.sigma.real());
            if (da == db) {
                return values[a] < values[b];
            }
            return da < db;
        }
        return orderDoubleEigenvalue(values[a], values[b], request.which);
    });
    SparseEigenResult result;
    result.vectors.resize(n, nev);
    result.values.resize(values.size());
    for (size_t dst = 0; dst < order.size(); ++dst) {
        size_t src = order[dst];
        result.values[dst] = values[src];
        for (int r = 0; r < n; ++r) {
            result.vectors(r, static_cast<Eigen::Index>(dst)) = vectors[r + n * src];
        }
    }
    return result;
}
//=============================================================================
static SparseGeneralEigenResult
sparseGeneralEigs(const SparseDoubleMatrix& mat, indexType k, const SparseEigsRequest& request)
{
    int n = static_cast<int>(mat.rows());
    int nev = static_cast<int>(k);
    int ncv = std::min(n, std::max(20, std::max(nev + 2, 2 * nev + 1)));
    int ido = 0;
    int info = 0;
    int ldv = n;
    int lworkl = 3 * ncv * (ncv + 2);
    char bmat = 'I';
    std::string arpackWhich = request.shiftInvert ? "LM" : getGeneralArpackWhich(request.which);
    std::unique_ptr<SparseLuSolver> shiftSolver;
    if (request.shiftInvert) {
        shiftSolver = factorizeShiftedSparseMatrix(mat, request.sigma.real());
    }
    double tol = 0.0;
    std::vector<double> resid(n, 0.0);
    std::vector<double> v(static_cast<size_t>(n) * ncv, 0.0);
    int iparam[11] = {};
    int ipntr[14] = {};
    std::vector<double> workd(static_cast<size_t>(3) * n, 0.0);
    std::vector<double> workl(lworkl, 0.0);
    iparam[0] = 1;
    iparam[2] = 300;
    iparam[6] = request.shiftInvert ? 3 : 1;
    do {
        dnaupd_(&ido, &bmat, &n, arpackWhich.data(), &nev, &tol, resid.data(), &ncv, v.data(), &ldv,
            iparam, ipntr, workd.data(), workl.data(), &lworkl, &info, 1, 2);
        if (ido == 1 || ido == -1) {
            Eigen::Map<const Eigen::VectorXd> x(&workd[ipntr[0] - 1], n);
            Eigen::Map<Eigen::VectorXd> y(&workd[ipntr[1] - 1], n);
            if (request.shiftInvert) {
                y = shiftSolver->solve(x);
                if (shiftSolver->info() != Eigen::Success) {
                    Error(L"Nelson:eigs:linearSolverFailed", L"%s failed.",
                        L"Sparse linear equation solver");
                }
            } else {
                y.noalias() = mat * x;
            }
        }
    } while (ido == 1 || ido == -1);
    if (info != 0) {
        Error(L"Nelson:eigs:solverFailed", L"%s failed.", L"ARPACK eigensolver");
    }
    int rvec = 1;
    char howmny = 'A';
    std::vector<int> select(ncv, 0);
    std::vector<double> realParts(nev + 1, 0.0);
    std::vector<double> imagParts(nev + 1, 0.0);
    std::vector<double> vectors(static_cast<size_t>(n) * (nev + 1), 0.0);
    std::vector<double> workev(static_cast<size_t>(3) * ncv, 0.0);
    double sigmar = request.shiftInvert ? request.sigma.real() : 0.0;
    double sigmai = 0.0;
    dneupd_(&rvec, &howmny, select.data(), realParts.data(), imagParts.data(), vectors.data(), &ldv,
        &sigmar, &sigmai, workev.data(), &bmat, &n, arpackWhich.data(), &nev, &tol, resid.data(),
        &ncv, v.data(), &ldv, iparam, ipntr, workd.data(), workl.data(), &lworkl, &info, 1, 1, 2);
    if (info != 0) {
        Error(L"Nelson:eigs:eigenvectorExtractionFailed", L"%s failed.",
            L"ARPACK eigenvector extraction");
    }
    SparseGeneralEigenResult raw;
    raw.values.reserve(nev);
    raw.vectors.resize(n, nev);
    int dst = 0;
    int src = 0;
    while (dst < nev && src < nev) {
        if (imagParts[src] > 0.0 && src + 1 < nev + 1 && dst + 1 < nev) {
            for (int r = 0; r < n; ++r) {
                double re = vectors[r + n * src];
                double im = vectors[r + n * (src + 1)];
                raw.vectors(r, dst) = std::complex<double>(re, im);
                raw.vectors(r, dst + 1) = std::complex<double>(re, -im);
            }
            raw.values.emplace_back(realParts[src], imagParts[src]);
            raw.values.emplace_back(realParts[src], -imagParts[src]);
            src += 2;
            dst += 2;
        } else {
            for (int r = 0; r < n; ++r) {
                raw.vectors(r, dst) = std::complex<double>(vectors[r + n * src], 0.0);
            }
            raw.values.emplace_back(realParts[src], imagParts[src]);
            ++src;
            ++dst;
        }
    }
    std::vector<size_t> order(raw.values.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&raw, &request](size_t a, size_t b) {
        if (request.shiftInvert) {
            double da = std::abs(raw.values[a] - request.sigma);
            double db = std::abs(raw.values[b] - request.sigma);
            if (da == db) {
                return raw.values[a].real() > raw.values[b].real();
            }
            return da < db;
        }
        return orderComplexEigenvalue(raw.values[a], raw.values[b], request.which);
    });
    SparseGeneralEigenResult result;
    result.vectors.resize(n, (Eigen::Index)order.size());
    result.values.resize(order.size());
    for (size_t dstIndex = 0; dstIndex < order.size(); ++dstIndex) {
        size_t srcIndex = order[dstIndex];
        result.values[dstIndex] = raw.values[srcIndex];
        result.vectors.col((Eigen::Index)dstIndex) = raw.vectors.col((Eigen::Index)srcIndex);
    }
    return result;
}
//=============================================================================
static SparseGeneralEigenResult
sparseComplexEigs(const SparseComplexMatrix& mat, indexType k, const SparseEigsRequest& request)
{
    int n = static_cast<int>(mat.rows());
    int nev = static_cast<int>(k);
    int ncv = std::min(n, std::max(20, std::max(nev + 2, 2 * nev + 1)));
    int ido = 0;
    int info = 0;
    int ldv = n;
    int lworkl = 3 * ncv * ncv + 5 * ncv;
    char bmat = 'I';
    std::string arpackWhich = request.shiftInvert ? "LM" : getGeneralArpackWhich(request.which);
    std::unique_ptr<SparseComplexLuSolver> shiftSolver;
    if (request.shiftInvert) {
        shiftSolver = factorizeShiftedSparseComplexMatrix(mat, request.sigma);
    }
    double tol = 0.0;
    std::vector<doublecomplex> resid(n, 0.0);
    std::vector<doublecomplex> v(static_cast<size_t>(n) * ncv, 0.0);
    int iparam[11] = {};
    int ipntr[14] = {};
    std::vector<doublecomplex> workd(static_cast<size_t>(3) * n, 0.0);
    std::vector<doublecomplex> workl(lworkl, 0.0);
    std::vector<double> rwork(ncv, 0.0);
    iparam[0] = 1;
    iparam[2] = 300;
    iparam[6] = request.shiftInvert ? 3 : 1;
    do {
        znaupd_(&ido, &bmat, &n, arpackWhich.data(), &nev, &tol, resid.data(), &ncv, v.data(), &ldv,
            iparam, ipntr, workd.data(), workl.data(), &lworkl, rwork.data(), &info, 1, 2);
        if (ido == 1 || ido == -1) {
            Eigen::Map<const Eigen::VectorXcd> x(&workd[ipntr[0] - 1], n);
            Eigen::Map<Eigen::VectorXcd> y(&workd[ipntr[1] - 1], n);
            if (request.shiftInvert) {
                y = shiftSolver->solve(x);
                if (shiftSolver->info() != Eigen::Success) {
                    Error(L"Nelson:eigs:linearSolverFailed", L"%s failed.",
                        L"Sparse linear equation solver");
                }
            } else {
                y.noalias() = mat * x;
            }
        }
    } while (ido == 1 || ido == -1);
    if (info != 0) {
        Error(L"Nelson:eigs:solverFailed", L"%s failed.", L"ARPACK eigensolver");
    }
    int rvec = 1;
    char howmny = 'A';
    std::vector<int> select(ncv, 0);
    std::vector<doublecomplex> values(nev, 0.0);
    std::vector<doublecomplex> vectors(static_cast<size_t>(n) * nev, 0.0);
    std::vector<doublecomplex> workev(static_cast<size_t>(2) * ncv, 0.0);
    doublecomplex sigma = request.shiftInvert ? request.sigma : doublecomplex(0.0, 0.0);
    zneupd_(&rvec, &howmny, select.data(), values.data(), vectors.data(), &ldv, &sigma,
        workev.data(), &bmat, &n, arpackWhich.data(), &nev, &tol, resid.data(), &ncv, v.data(),
        &ldv, iparam, ipntr, workd.data(), workl.data(), &lworkl, rwork.data(), &info, 1, 1, 2);
    if (info != 0) {
        Error(L"Nelson:eigs:eigenvectorExtractionFailed", L"%s failed.",
            L"ARPACK eigenvector extraction");
    }
    std::vector<size_t> order(values.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(order.begin(), order.end(), [&values, &request](size_t a, size_t b) {
        if (request.shiftInvert) {
            double da = std::abs(values[a] - request.sigma);
            double db = std::abs(values[b] - request.sigma);
            if (da == db) {
                return values[a].real() > values[b].real();
            }
            return da < db;
        }
        return orderComplexEigenvalue(values[a], values[b], request.which);
    });
    SparseGeneralEigenResult result;
    result.vectors.resize(n, nev);
    result.values.resize(values.size());
    for (size_t dst = 0; dst < order.size(); ++dst) {
        size_t src = order[dst];
        result.values[dst] = values[src];
        for (int r = 0; r < n; ++r) {
            result.vectors(r, static_cast<Eigen::Index>(dst)) = vectors[r + n * src];
        }
    }
    return result;
}
//=============================================================================
#include "SparseArpackComplexHelpers.hpp"
#endif
//=============================================================================
static ArrayOf
makeDoubleColumn(const std::vector<double>& values)
{
    auto* ptr = (double*)new_with_exception<double>(values.size(), false);
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions((indexType)values.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
makeDoubleDenseMatrix(const Eigen::MatrixXd& mat)
{
    auto* ptr = (double*)new_with_exception<double>((size_t)mat.rows() * mat.cols(), false);
    std::copy(mat.data(), mat.data() + mat.size(), ptr);
    return ArrayOf(NLS_DOUBLE, Dimensions((indexType)mat.rows(), (indexType)mat.cols()), ptr);
}
//=============================================================================
static bool
allReal(const std::vector<std::complex<double>>& values, double tolerance)
{
    for (const auto& value : values) {
        if (std::abs(value.imag()) > tolerance) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static ArrayOf
makeComplexColumn(const std::vector<std::complex<double>>& values)
{
    if (allReal(values, 1e-14)) {
        std::vector<double> realValues(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            realValues[k] = values[k].real();
        }
        return makeDoubleColumn(realValues);
    }
    auto* ptr = (doublecomplex*)new_with_exception<doublecomplex>(values.size(), false);
    std::copy(values.begin(), values.end(), ptr);
    return ArrayOf(NLS_DCOMPLEX, Dimensions((indexType)values.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
makeComplexDenseMatrix(const Eigen::MatrixXcd& mat)
{
    if (mat.imag().cwiseAbs().maxCoeff() <= 1e-14) {
        return makeDoubleDenseMatrix(mat.real());
    }
    auto* ptr
        = (doublecomplex*)new_with_exception<doublecomplex>((size_t)mat.rows() * mat.cols(), false);
    std::copy(mat.data(), mat.data() + mat.size(), ptr);
    return ArrayOf(NLS_DCOMPLEX, Dimensions((indexType)mat.rows(), (indexType)mat.cols()), ptr);
}
//=============================================================================
static ArrayOf
makeDoubleDiagonal(const std::vector<double>& values)
{
    const indexType n = (indexType)values.size();
    auto* ptr = (double*)new_with_exception<double>((size_t)n * n, true);
    for (indexType i = 0; i < n; ++i) {
        ptr[i + n * i] = values[i];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, n), ptr);
}
//=============================================================================
static ArrayOf
makeComplexDiagonal(const std::vector<std::complex<double>>& values)
{
    if (allReal(values, 1e-14)) {
        std::vector<double> realValues(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            realValues[k] = values[k].real();
        }
        return makeDoubleDiagonal(realValues);
    }
    const indexType n = (indexType)values.size();
    auto* ptr = (doublecomplex*)new_with_exception<doublecomplex>((size_t)n * n, true);
    for (indexType i = 0; i < n; ++i) {
        ptr[i + n * i] = values[i];
    }
    return ArrayOf(NLS_DCOMPLEX, Dimensions(n, n), ptr);
}
//=============================================================================
ArrayOfVector
EigsSparseDouble(int nLhs, const ArrayOfVector& argIn)
{
#if WITH_ARPACK
    ArrayOf A(argIn[0]);
    if (A.dataClass() != NLS_DOUBLE && A.dataClass() != NLS_DCOMPLEX) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double matrix");
    }
    if (!A.is2D() || !A.isSquare()) {
        Error(L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected.", L"Square matrix");
    }
    indexType k = getSparseArpackK(argIn, A.rows());
    SparseEigsRequest request = getSparseEigsRequest(argIn);
    if (A.dataClass() == NLS_DCOMPLEX) {
        auto* mat = (SparseComplexMatrix*)A.sparseDataPointer();
        if (!isFiniteSparseComplex(*mat)) {
            Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
                L"Input argument");
        }
        SparseGeneralEigenResult result = sparseComplexEigs(*mat, k, request);
        ArrayOfVector retval;
        if (nLhs <= 1) {
            retval << makeComplexColumn(result.values);
        } else {
            retval << makeComplexDenseMatrix(result.vectors);
            retval << makeComplexDiagonal(result.values);
        }
        return retval;
    }
    if (request.sigma.imag() != 0.0) {
        Error(L"Nelson:eigs:invalidSigma", L"%s must be a finite real scalar.", L"SIGMA");
    }
    auto* mat = (SparseDoubleMatrix*)A.sparseDataPointer();
    if (!isFiniteSparseDouble(*mat)) {
        Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
            L"Input argument");
    }
    if (!isSymmetricSparseDouble(*mat)) {
        SparseGeneralEigenResult result = sparseGeneralEigs(*mat, k, request);
        ArrayOfVector retval;
        if (nLhs <= 1) {
            retval << makeComplexColumn(result.values);
        } else {
            retval << makeComplexDenseMatrix(result.vectors);
            retval << makeComplexDiagonal(result.values);
        }
        return retval;
    }
    SparseEigenResult result = sparseSymmetricEigs(*mat, k, request);
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << makeDoubleColumn(result.values);
    } else {
        retval << makeDoubleDenseMatrix(result.vectors);
        retval << makeDoubleDiagonal(result.values);
    }
    return retval;
#else
    return EigsSparseDenseFallback(nLhs, argIn);
#endif
}
//=============================================================================
ArrayOfVector
SvdsSparseDouble(int nLhs, const ArrayOfVector& argIn)
{
#if WITH_ARPACK
    ArrayOf A(argIn[0]);
    if (A.dataClass() != NLS_DOUBLE && A.dataClass() != NLS_DCOMPLEX) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double matrix");
    }
    if (!A.is2D()) {
        Error(L"Nelson:linear_algebra:matrix2DExpected", L"%s expected.", L"Vector or matrix 2D");
    }
    indexType k = getSparseArpackK(argIn, std::min(A.rows(), A.columns()));
    bool useRightNormalMatrix = A.rows() >= A.columns();
    std::string which = getSparseArpackSingularWhich(argIn);
    SparseEigsRequest normalRequest { which, false, 0.0 };
    if (A.dataClass() == NLS_DCOMPLEX) {
        auto* mat = (SparseComplexMatrix*)A.sparseDataPointer();
        if (!isFiniteSparseComplex(*mat)) {
            Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
                L"Input argument");
        }
        SparseComplexMatrix normal;
        if (useRightNormalMatrix) {
            normal = mat->adjoint() * (*mat);
        } else {
            normal = (*mat) * mat->adjoint();
        }
        SparseGeneralEigenResult eigResult = sparseHermitianComplexEigs(normal, k, which);
        std::vector<double> singularValues(eigResult.values.size());
        for (size_t i = 0; i < eigResult.values.size(); ++i) {
            singularValues[i] = std::sqrt(std::max(0.0, eigResult.values[i].real()));
        }
        std::vector<size_t> singularOrder(singularValues.size());
        std::iota(singularOrder.begin(), singularOrder.end(), 0);
        std::sort(singularOrder.begin(), singularOrder.end(),
            [&singularValues](size_t left, size_t right) {
                return singularValues[left] > singularValues[right];
            });
        Eigen::MatrixXcd singularVectors(eigResult.vectors.rows(), eigResult.vectors.cols());
        std::vector<double> orderedSingularValues(singularValues.size());
        for (size_t i = 0; i < singularOrder.size(); ++i) {
            size_t sourceIndex = singularOrder[i];
            orderedSingularValues[i] = singularValues[sourceIndex];
            singularVectors.col((Eigen::Index)i) = eigResult.vectors.col((Eigen::Index)sourceIndex);
        }
        singularValues.swap(orderedSingularValues);
        eigResult.vectors.swap(singularVectors);
        ArrayOfVector retval;
        if (nLhs <= 1) {
            retval << makeDoubleColumn(singularValues);
        } else {
            if (useRightNormalMatrix) {
                Eigen::MatrixXcd U = (*mat) * eigResult.vectors;
                for (size_t i = 0; i < singularValues.size(); ++i) {
                    if (singularValues[i] > std::numeric_limits<double>::epsilon()) {
                        U.col((Eigen::Index)i) /= singularValues[i];
                    } else {
                        U.col((Eigen::Index)i).setZero();
                    }
                }
                retval << makeComplexDenseMatrix(U);
                retval << makeDoubleDiagonal(singularValues);
                retval << makeComplexDenseMatrix(eigResult.vectors);
            } else {
                Eigen::MatrixXcd V = mat->adjoint() * eigResult.vectors;
                for (size_t i = 0; i < singularValues.size(); ++i) {
                    if (singularValues[i] > std::numeric_limits<double>::epsilon()) {
                        V.col((Eigen::Index)i) /= singularValues[i];
                    } else {
                        V.col((Eigen::Index)i).setZero();
                    }
                }
                retval << makeComplexDenseMatrix(eigResult.vectors);
                retval << makeDoubleDiagonal(singularValues);
                retval << makeComplexDenseMatrix(V);
            }
        }
        return retval;
    }
    auto* mat = (SparseDoubleMatrix*)A.sparseDataPointer();
    if (!isFiniteSparseDouble(*mat)) {
        Error(L"Nelson:linear_algebra:inputMustBeFinite", L"%s must not contain NaN or Inf.",
            L"Input argument");
    }
    SparseDoubleMatrix normal;
    if (useRightNormalMatrix) {
        normal = mat->transpose() * (*mat);
    } else {
        normal = (*mat) * mat->transpose();
    }
    SparseEigenResult eigResult = sparseSymmetricEigs(normal, k, normalRequest);
    std::vector<double> singularValues(eigResult.values.size());
    for (size_t i = 0; i < eigResult.values.size(); ++i) {
        singularValues[i] = std::sqrt(std::max(0.0, eigResult.values[i]));
    }
    std::vector<size_t> singularOrder(singularValues.size());
    std::iota(singularOrder.begin(), singularOrder.end(), 0);
    std::sort(
        singularOrder.begin(), singularOrder.end(), [&singularValues](size_t left, size_t right) {
            return singularValues[left] > singularValues[right];
        });
    Eigen::MatrixXd singularVectors(eigResult.vectors.rows(), eigResult.vectors.cols());
    std::vector<double> orderedSingularValues(singularValues.size());
    for (size_t i = 0; i < singularOrder.size(); ++i) {
        size_t sourceIndex = singularOrder[i];
        orderedSingularValues[i] = singularValues[sourceIndex];
        singularVectors.col((Eigen::Index)i) = eigResult.vectors.col((Eigen::Index)sourceIndex);
    }
    singularValues.swap(orderedSingularValues);
    eigResult.vectors.swap(singularVectors);
    ArrayOfVector retval;
    if (nLhs <= 1) {
        retval << makeDoubleColumn(singularValues);
    } else {
        if (useRightNormalMatrix) {
            Eigen::MatrixXd U = (*mat) * eigResult.vectors;
            for (size_t i = 0; i < singularValues.size(); ++i) {
                if (singularValues[i] > std::numeric_limits<double>::epsilon()) {
                    U.col((Eigen::Index)i) /= singularValues[i];
                } else {
                    U.col((Eigen::Index)i).setZero();
                }
            }
            retval << makeDoubleDenseMatrix(U);
            retval << makeDoubleDiagonal(singularValues);
            retval << makeDoubleDenseMatrix(eigResult.vectors);
        } else {
            Eigen::MatrixXd V = mat->transpose() * eigResult.vectors;
            for (size_t i = 0; i < singularValues.size(); ++i) {
                if (singularValues[i] > std::numeric_limits<double>::epsilon()) {
                    V.col((Eigen::Index)i) /= singularValues[i];
                } else {
                    V.col((Eigen::Index)i).setZero();
                }
            }
            retval << makeDoubleDenseMatrix(eigResult.vectors);
            retval << makeDoubleDiagonal(singularValues);
            retval << makeDoubleDenseMatrix(V);
        }
    }
    return retval;
#else
    return SvdsSparseDenseFallback(nLhs, argIn);
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
