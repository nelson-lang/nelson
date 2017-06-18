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
#include "MinusDouble.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf double_subtraction(ArrayOf a, ArrayOf  b)
    {
        Dimensions Cdim;
        if (a.isEmpty())
        {
            Dimensions dimA = a.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            if (mA == nA)
            {
                if (b.isEmpty())
                {
                    Dimensions dimB = b.getDimensions();
                    size_t mB = dimB.getRows();
                    size_t nB = dimB.getColumns();
                    if ((mB == mA) && (nA == nB))
                    {
                        return ArrayOf(a);
                    }
                    else
                    {
                        throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
                    }
                }
                if (b.isScalar())
                {
                    // [] - X returns []
                    return ArrayOf(a);
                }
                else
                {
                    throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
                }
            }
        }
        if (a.isScalar())
        {
            Cdim = b.getDimensions();
        }
        else
        {
            Cdim = a.getDimensions();
        }
        indexType Clen = Cdim.getElementCount();
        void *Cp = new_with_exception<double>(Clen);
        size_t mC = Cdim.getRows();
        size_t nC = Cdim.getColumns();
        Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
        Dimensions dimA = a.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = b.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (a.isScalar())
        {
            Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), mB, nB);
            matC = a.getContentAsDoubleScalar() - matB.array();
        }
        else if (b.isScalar())
        {
            Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), mA, nA);
            matC = matA.array() - b.getContentAsDoubleScalar();
        }
        else
        {
            Eigen::Map<Eigen::MatrixXd> matA((double*)a.getDataPointer(), mA, nA);
            Eigen::Map<Eigen::MatrixXd> matB((double*)b.getDataPointer(), mB, nB);
            matC = matA - matB;
        }
        return ArrayOf(NLS_DOUBLE, Cdim, Cp, false);
    }
    //=============================================================================
    ArrayOf dcomplex_subtraction(ArrayOf a, ArrayOf  b)
    {
        if (a.isEmpty())
        {
            Dimensions dimA = a.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            if (mA == nA)
            {
                if (b.isEmpty())
                {
                    Dimensions dimB = b.getDimensions();
                    size_t mB = dimB.getRows();
                    size_t nB = dimB.getColumns();
                    if ((mB == mA) && (nA == nB))
                    {
                        return ArrayOf(a);
                    }
                    else
                    {
                        throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
                    }
                }
                if (b.isScalar())
                {
                    // [] - X returns []
                    return ArrayOf(a);
                }
                else
                {
                    throw Exception(_W("using operator '-' \n Matrix dimensions must agree."));
                }
            }
        }
        Dimensions Cdim;
        a.promoteType(NLS_DCOMPLEX);
        b.promoteType(NLS_DCOMPLEX);
        if (a.isScalar())
        {
            Cdim = b.getDimensions();
        }
        else
        {
            Cdim = a.getDimensions();
        }
        indexType Clen = Cdim.getElementCount();
        void *Cp = new_with_exception<double>(Clen * 2);
        doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
        size_t mC = Cdim.getRows();
        size_t nC = Cdim.getColumns();
        Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
        Dimensions dimA = a.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = b.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (a.isScalar())
        {
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
            if (b.getDataClass() == NLS_DCOMPLEX)
            {
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                matC = Az[0] - matB.array();
            }
            else
            {
                double *Bz = (double*)b.getDataPointer();
                Eigen::Map<Eigen::MatrixXd> matB(Bz, mB, nB);
                matC = Az[0] - matB.cast<doublecomplex>().array();
            }
        }
        else if (b.isScalar())
        {
            doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
            if (a.getDataClass() == NLS_DCOMPLEX)
            {
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                matC = matA.array() - Bz[0];
            }
            else
            {
                double *Az = (double*)a.getDataPointer();
                Eigen::Map<Eigen::MatrixXd> matA(Az, mA, nA);
                matC = matA.cast<doublecomplex>().array() - Bz[0];
            }
        }
        else
        {
            doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)a.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
            doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)b.getDataPointer());
            Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
            matC = matA - matB;
        }
        ArrayOf res = ArrayOf(NLS_DCOMPLEX, Cdim, Cp, false);
        if (res.allReal())
        {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    }
    //=============================================================================
    ArrayOf double_minus_double(ArrayOf a, ArrayOf b)
    {
        if (a.isComplex() || b.isComplex())
        {
            return dcomplex_subtraction(a, b);
        }
        return double_subtraction(a, b);
    }
    //=============================================================================

}
//=============================================================================
