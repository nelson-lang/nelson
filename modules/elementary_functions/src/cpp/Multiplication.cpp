//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "Multiplication.hpp"
#include "Sparse.hpp"
#include "MatrixCheck.hpp"
#include "DotMultiply.hpp"
//=============================================================================
namespace NelSon {

    Array Multiply2D(Array A, Array B) throw(Exception)
    {
        if (A.isEmpty() || B.isEmpty())
        {
            return Array::emptyConstructor();
        }
        // Test for conformancy
        if (A.getDimensionLength(1) != B.getDimensionLength(0))
        {
            throw Exception("Requested matrix multiplication requires arguments to be conformant.");
        }
        int Arows;
        int Bcols;
        Arows = A.getDimensionLength(0);
        Bcols = B.getDimensionLength(1);
        Dimensions outDim(2);
        outDim[0] = Arows;
        outDim[1] = Bcols;
        // Its really a matrix-matrix operation, and the arguments are
        // satisfactory.  Check for the type.
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = B.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        size_t mC = outDim.getRows();
        size_t nC = outDim.getColumns();
        void *Cp = NULL;
        switch (A.getDataClass())
        {
            case NLS_SINGLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float * double returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB.cast<float>();
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float * double complex return float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DOUBLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA.cast<float>() * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double * double returns double
                        Cp = Array::allocateArray(NLS_DOUBLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DOUBLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<double>>() * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_SCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float complex * double returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float complex * float complex returs float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float complex * double complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double complex * double returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<double>>();
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double complex * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double complex * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            default:
            {
                throw Exception("Matrix multiplication not implemented in NelSon for these types.");
            }
            break;
        }
    }

    Array MultiplySparse(Array A, Array B) throw(Exception)
    {
        // Test for conformancy
        if (A.getDimensionLength(1) != B.getDimensionLength(0))
        {
            throw Exception("Requested matrix multiplication requires arguments to be conformant.");
        }
        int Arows = A.getDimensionLength(0);
        int Acols = A.getDimensionLength(1);
        int Bcols = B.getDimensionLength(1);
        Dimensions outDim(2);
        outDim[0] = Arows;
        outDim[1] = Bcols;
        if (A.isSparse() || B.isSparse())
        {
            // Check for sparse multiply case
            if (A.isSparse() && !B.isSparse())
            {
                void *pRes = SparseDenseMatrixMultiply(A.getDataClass(), Arows, Acols, Bcols, A.getSparseDataPointer(), B.getDataPointer());
                return Array(A.getDataClass(), outDim, pRes, false);
            }
            if (!A.isSparse() && B.isSparse())
            {
                void *pRes = DenseSparseMatrixMultiply(A.getDataClass(), Arows, Acols, Bcols, A.getDataPointer(), B.getSparseDataPointer());
                return Array(A.getDataClass(), outDim, pRes, false);
            }
            if (A.isSparse() && B.isSparse())
            {
                void *pRes = SparseSparseMatrixMultiply(A.getDataClass(), Arows, Acols, Bcols, A.getSparseDataPointer(), B.getSparseDataPointer());
                return Array(A.getDataClass(), outDim, pRes, true);
            }
        }
        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
    }

    Array MultiplyOperator(Array A, Array B) throw(Exception)
    {
        if (A.isEmpty() || B.isEmpty())
        {
            return Array::emptyConstructor();
        }
        // Process our arguments
        if (!MatrixCheck(A, B, "*"))
            // Its really a vector product, pass...
        {
            return DotMultiply(A, B);
        }
        // Test for conformancy
        if (A.getDimensionLength(1) != B.getDimensionLength(0))
        {
            throw Exception("Requested matrix multiplication requires arguments to be conformant.");
        }
        int Arows, Acols;
        int Bcols;
        Arows = A.getDimensionLength(0);
        Acols = A.getDimensionLength(1);
        Bcols = B.getDimensionLength(1);
        Dimensions outDim(2);
        outDim[0] = Arows;
        outDim[1] = Bcols;
        if (A.isSparse() || B.isSparse())
        {
            // Check for sparse multiply case
            if (A.isSparse() && !B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             SparseDenseMatrixMultiply(A.getDataClass(),
                                                       Arows, Acols, Bcols,
                                                       A.getSparseDataPointer(),
                                                       B.getDataPointer()),
                             false);
            if (!A.isSparse() && B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             DenseSparseMatrixMultiply(A.getDataClass(),
                                                       Arows, Acols, Bcols,
                                                       A.getDataPointer(),
                                                       B.getSparseDataPointer()),
                             false);
            if (A.isSparse() && B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             SparseSparseMatrixMultiply(A.getDataClass(),
                                                        Arows, Acols, Bcols,
                                                        A.getSparseDataPointer(),
                                                        B.getSparseDataPointer()),
                             true);
        }
        // Its really a matrix-matrix operation, and the arguments are
        // satisfactory.  Check for the type.
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = B.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        size_t mC = outDim.getRows();
        size_t nC = outDim.getColumns();
        void *Cp = NULL;
        switch (A.getDataClass())
        {
            case NLS_SINGLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float * double returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB.cast<float>();
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float * double complex return float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DOUBLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA.cast<float>() * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double * double returns double
                        Cp = Array::allocateArray(NLS_DOUBLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DOUBLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<double>>() * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_SCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float complex * double returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float complex * float complex returs float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float complex * double complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double complex * double returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<double>>();
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double complex * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double complex * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            default:
            {
                throw Exception("Matrix multiplication not implemented in NelSon for these types.");
            }
            break;
        }
    }

    Array Multiply(Array A, Array B) throw(Exception)
    {
        if (A.isEmpty() || B.isEmpty())
        {
            return Array::emptyConstructor();
        }
        // Process our arguments
        if (!MatrixCheck(A, B, "*"))
            // Its really a vector product, pass...
        {
            return DotMultiply(A, B);
        }
        // Test for conformancy
        if (A.getDimensionLength(1) != B.getDimensionLength(0))
        {
            throw Exception("Requested matrix multiplication requires arguments to be conformant.");
        }
        int Arows, Acols;
        int Bcols;
        Arows = A.getDimensionLength(0);
        Acols = A.getDimensionLength(1);
        Bcols = B.getDimensionLength(1);
        Dimensions outDim(2);
        outDim[0] = Arows;
        outDim[1] = Bcols;
        if (A.isSparse() || B.isSparse())
        {
            // Check for sparse multiply case
            if (A.isSparse() && !B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             SparseDenseMatrixMultiply(A.getDataClass(),
                                                       Arows, Acols, Bcols,
                                                       A.getSparseDataPointer(),
                                                       B.getDataPointer()),
                             false);
            if (!A.isSparse() && B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             DenseSparseMatrixMultiply(A.getDataClass(),
                                                       Arows, Acols, Bcols,
                                                       A.getDataPointer(),
                                                       B.getSparseDataPointer()),
                             false);
            if (A.isSparse() && B.isSparse())
                return Array(A.getDataClass(),
                             outDim,
                             SparseSparseMatrixMultiply(A.getDataClass(),
                                                        Arows, Acols, Bcols,
                                                        A.getSparseDataPointer(),
                                                        B.getSparseDataPointer()),
                             true);
        }
        // Its really a matrix-matrix operation, and the arguments are
        // satisfactory.  Check for the type.
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = B.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        size_t mC = outDim.getRows();
        size_t nC = outDim.getColumns();
        void *Cp = NULL;
        switch (A.getDataClass())
        {
            case NLS_SINGLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float * double returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA * matB.cast<float>();
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float * double complex return float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXf> matA((float*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DOUBLE:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double * float returns float
                        Cp = Array::allocateArray(NLS_SINGLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXf> matC((float*)Cp, mC, nC);
                        matC = matA.cast<float>() * matB;
                        return Array(NLS_SINGLE, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double * double returns double
                        Cp = Array::allocateArray(NLS_DOUBLE, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DOUBLE, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<double>>() * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_SCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // float complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // float complex * double returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // float complex * float complex returs float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // float complex * double complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<float>* Az = reinterpret_cast<std::complex<float>*>((float*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            case NLS_DCOMPLEX:
            {
                switch (B.getDataClass())
                {
                    case NLS_SINGLE:
                    {
                        // double complex * float returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXf> matB((float*)B.getDataPointer(), mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB.cast<std::complex<float>>();
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DOUBLE:
                    {
                        // double complex * double returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB.cast<std::complex<double>>();
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_SCOMPLEX:
                    {
                        // double complex * float complex returns float complex
                        Cp = Array::allocateArray(NLS_SCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        std::complex<float>* Bz = reinterpret_cast<std::complex<float>*>((float*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                        std::complex<float>* Cz = reinterpret_cast<std::complex<float>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
                        matC = matA.cast<std::complex<float>>() * matB;
                        return Array(NLS_SCOMPLEX, outDim, Cp);
                    }
                    break;
                    case NLS_DCOMPLEX:
                    {
                        // double complex * double complex returns double complex
                        Cp = Array::allocateArray(NLS_DCOMPLEX, mC * nC);
                        std::complex<double>* Az = reinterpret_cast<std::complex<double>*>((double*)A.getDataPointer());
                        std::complex<double>* Bz = reinterpret_cast<std::complex<double>*>((double*)B.getDataPointer());
                        Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                        Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                        std::complex<double>* Cz = reinterpret_cast<std::complex<double>*>(Cp);
                        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
                        matC = matA * matB;
                        return Array(NLS_DCOMPLEX, outDim, Cp);
                    }
                    break;
                    default:
                    {
                        throw Exception("Matrix multiplication not implemented in NelSon for these types.");
                    }
                    break;
                }
            }
            break;
            default:
            {
                throw Exception("Matrix multiplication not implemented in NelSon for these types.");
            }
            break;
        }
    }

}
