//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#ifdef NELSON_WITH_SUNDIALS
//=============================================================================
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cvodes/cvodes.h>
#include <idas/idas.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_context.h>
#include <sundials/sundials_linearsolver.h>
#include <sundials/sundials_matrix.h>
#include <sunlinsol/sunlinsol_dense.h>
#ifdef NELSON_WITH_SUNDIALS_PCG
#include <sunlinsol/sunlinsol_pcg.h>
#endif
#ifdef NELSON_WITH_SUNDIALS_SPBCGS
#include <sunlinsol/sunlinsol_spbcgs.h>
#endif
#ifdef NELSON_WITH_SUNDIALS_SPFGMR
#include <sunlinsol/sunlinsol_spfgmr.h>
#endif
#ifdef NELSON_WITH_SUNDIALS_SPGMR
#include <sunlinsol/sunlinsol_spgmr.h>
#endif
#ifdef NELSON_WITH_SUNDIALS_SPTFQMR
#include <sunlinsol/sunlinsol_sptfqmr.h>
#endif
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
#include <sunlinsol/sunlinsol_klu.h>
#endif
#include <sunmatrix/sunmatrix_dense.h>
#ifdef NELSON_WITH_SUNDIALS_SPARSE_MATRIX
#include <sunmatrix/sunmatrix_sparse.h>
#endif
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::OdeSolversGateway::SundialsDetail {
//=============================================================================
class SunContextHandle
{
public:
    SunContextHandle()
    {
        if (SUNContext_Create(SUN_COMM_NULL, &value) < 0) {
            Error(L"Nelson:ode_solvers:sundialsContextFailed",
                L"Unable to initialize the SUNDIALS solver.");
        }
    }

    ~SunContextHandle()
    {
        if (value != nullptr) {
            SUNContext_Free(&value);
        }
    }

    SunContextHandle(const SunContextHandle&) = delete;
    SunContextHandle&
    operator=(const SunContextHandle&)
        = delete;

    SUNContext
    get() const
    {
        return value;
    }

private:
    SUNContext value { nullptr };
};
//=============================================================================
class SerialVectorHandle
{
public:
    SerialVectorHandle(sunindextype size, SUNContext sunctx)
    {
        value = N_VNew_Serial(size, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsVectorAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    SerialVectorHandle(const std::vector<double>& values, SUNContext sunctx)
        : SerialVectorHandle(static_cast<sunindextype>(values.size()), sunctx)
    {
        std::copy(values.begin(), values.end(), data());
    }

    ~SerialVectorHandle()
    {
        if (value != nullptr) {
            N_VDestroy_Serial(value);
        }
    }

    SerialVectorHandle(const SerialVectorHandle&) = delete;
    SerialVectorHandle&
    operator=(const SerialVectorHandle&)
        = delete;

    N_Vector
    get() const
    {
        return value;
    }

    double*
    data() const
    {
        return NV_DATA_S(value);
    }

private:
    N_Vector value { nullptr };
};
//=============================================================================
class SerialVectorArrayHandle
{
public:
    SerialVectorArrayHandle(indexType count, sunindextype size, SUNContext sunctx)
    {
        values.reserve(static_cast<size_t>(count));
        for (indexType k = 0; k < count; ++k) {
            N_Vector vector = N_VNew_Serial(size, sunctx);
            if (vector == nullptr) {
                destroyValues();
                Error(L"Nelson:ode_solvers:sundialsVectorAllocationFailed",
                    L"Unable to allocate SUNDIALS memory.");
            }
            values.push_back(vector);
        }
    }

    SerialVectorArrayHandle(
        indexType count, sunindextype size, const std::vector<double>& data, SUNContext sunctx)
        : SerialVectorArrayHandle(count, size, sunctx)
    {
        const size_t block = static_cast<size_t>(size);
        if (data.size() != static_cast<size_t>(count) * block) {
            Error(L"Nelson:ode_solvers:sundialsVectorArraySizeMismatch",
                L"SUNDIALS vector array has incompatible size.");
        }
        for (indexType k = 0; k < count; ++k) {
            std::copy(data.begin() + static_cast<size_t>(k) * block,
                data.begin() + static_cast<size_t>(k + 1) * block, NV_DATA_S(values[k]));
        }
    }

    ~SerialVectorArrayHandle() { destroyValues(); }

    SerialVectorArrayHandle(const SerialVectorArrayHandle&) = delete;
    SerialVectorArrayHandle&
    operator=(const SerialVectorArrayHandle&)
        = delete;

    N_Vector*
    data()
    {
        return values.data();
    }

private:
    void
    destroyValues()
    {
        for (N_Vector vector : values) {
            if (vector != nullptr) {
                N_VDestroy_Serial(vector);
            }
        }
        values.clear();
    }

    std::vector<N_Vector> values;
};
//=============================================================================
class DenseMatrixHandle
{
public:
    DenseMatrixHandle(sunindextype rows, sunindextype columns, SUNContext sunctx)
    {
        value = SUNDenseMatrix(rows, columns, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~DenseMatrixHandle()
    {
        if (value != nullptr) {
            SUNMatDestroy(value);
        }
    }

    DenseMatrixHandle(const DenseMatrixHandle&) = delete;
    DenseMatrixHandle&
    operator=(const DenseMatrixHandle&)
        = delete;

    SUNMatrix
    get() const
    {
        return value;
    }

private:
    SUNMatrix value { nullptr };
};
//=============================================================================
#ifdef NELSON_WITH_SUNDIALS_SPARSE_MATRIX
class SparseMatrixHandle
{
public:
    SparseMatrixHandle(
        sunindextype rows, sunindextype columns, sunindextype nonzeros, SUNContext sunctx)
    {
#ifdef SUN_CSC_MAT
        constexpr int sparseFormat = SUN_CSC_MAT;
#else
        constexpr int sparseFormat = CSC_MAT;
#endif
        value = SUNSparseMatrix(rows, columns, nonzeros, sparseFormat, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~SparseMatrixHandle()
    {
        if (value != nullptr) {
            SUNMatDestroy(value);
        }
    }

    SparseMatrixHandle(const SparseMatrixHandle&) = delete;
    SparseMatrixHandle&
    operator=(const SparseMatrixHandle&)
        = delete;

    SUNMatrix
    get() const
    {
        return value;
    }

private:
    SUNMatrix value { nullptr };
};
//=============================================================================
#endif
//=============================================================================
class DenseLinearSolverHandle
{
public:
    DenseLinearSolverHandle(N_Vector y, SUNMatrix matrix, SUNContext sunctx)
    {
        value = SUNLinSol_Dense(y, matrix, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~DenseLinearSolverHandle()
    {
        if (value != nullptr) {
            SUNLinSolFree(value);
        }
    }

    DenseLinearSolverHandle(const DenseLinearSolverHandle&) = delete;
    DenseLinearSolverHandle&
    operator=(const DenseLinearSolverHandle&)
        = delete;

    SUNLinearSolver
    get() const
    {
        return value;
    }

private:
    SUNLinearSolver value { nullptr };
};
//=============================================================================
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
class SparseDirectLinearSolverHandle
{
public:
    SparseDirectLinearSolverHandle(N_Vector y, SUNMatrix matrix, SUNContext sunctx)
    {
        value = SUNLinSol_KLU(y, matrix, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~SparseDirectLinearSolverHandle()
    {
        if (value != nullptr) {
            SUNLinSolFree(value);
        }
    }

    SparseDirectLinearSolverHandle(const SparseDirectLinearSolverHandle&) = delete;
    SparseDirectLinearSolverHandle&
    operator=(const SparseDirectLinearSolverHandle&)
        = delete;

    SUNLinearSolver
    get() const
    {
        return value;
    }

private:
    SUNLinearSolver value { nullptr };
};
//=============================================================================
#endif
//=============================================================================
class IterativeLinearSolverHandle
{
public:
    IterativeLinearSolverHandle(
        N_Vector y, const std::string& solverName, bool preconditioned, SUNContext sunctx)
    {
        const int pretype = preconditioned ? SUN_PREC_LEFT : SUN_PREC_NONE;
#ifdef NELSON_WITH_SUNDIALS_SPGMR
        if (solverName == "spgmr") {
            value = SUNLinSol_SPGMR(y, pretype, 0, sunctx);
        }
#endif
#ifdef NELSON_WITH_SUNDIALS_SPFGMR
        if (solverName == "spfgmr") {
            value = SUNLinSol_SPFGMR(y, pretype, 0, sunctx);
        }
#endif
#ifdef NELSON_WITH_SUNDIALS_SPBCGS
        if (solverName == "spbcgs") {
            value = SUNLinSol_SPBCGS(y, pretype, 0, sunctx);
        }
#endif
#ifdef NELSON_WITH_SUNDIALS_SPTFQMR
        if (solverName == "sptfqmr") {
            value = SUNLinSol_SPTFQMR(y, pretype, 0, sunctx);
        }
#endif
#ifdef NELSON_WITH_SUNDIALS_PCG
        if (solverName == "pcg") {
            value = SUNLinSol_PCG(y, pretype, 0, sunctx);
        }
#endif
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~IterativeLinearSolverHandle()
    {
        if (value != nullptr) {
            SUNLinSolFree(value);
        }
    }

    IterativeLinearSolverHandle(const IterativeLinearSolverHandle&) = delete;
    IterativeLinearSolverHandle&
    operator=(const IterativeLinearSolverHandle&)
        = delete;

    SUNLinearSolver
    get() const
    {
        return value;
    }

private:
    SUNLinearSolver value { nullptr };
};
//=============================================================================
class CvodesMemoryHandle
{
public:
    CvodesMemoryHandle(int method, SUNContext sunctx)
    {
        value = CVodeCreate(method, sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~CvodesMemoryHandle()
    {
        if (value != nullptr) {
            CVodeFree(&value);
        }
    }

    CvodesMemoryHandle(const CvodesMemoryHandle&) = delete;
    CvodesMemoryHandle&
    operator=(const CvodesMemoryHandle&)
        = delete;

    void*
    get() const
    {
        return value;
    }

private:
    void* value { nullptr };
};
//=============================================================================
class IdasMemoryHandle
{
public:
    explicit IdasMemoryHandle(SUNContext sunctx)
    {
        value = IDACreate(sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~IdasMemoryHandle()
    {
        if (value != nullptr) {
            IDAFree(&value);
        }
    }

    IdasMemoryHandle(const IdasMemoryHandle&) = delete;
    IdasMemoryHandle&
    operator=(const IdasMemoryHandle&)
        = delete;

    void*
    get() const
    {
        return value;
    }

private:
    void* value { nullptr };
};
//=============================================================================
} // namespace Nelson::OdeSolversGateway::SundialsDetail
//=============================================================================
#endif
//=============================================================================
