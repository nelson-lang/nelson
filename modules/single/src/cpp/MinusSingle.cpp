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
#include "MinusSingle.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf single_subtraction(ArrayOf a, ArrayOf  b)
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
        void *Cp = new_with_exception<single>(Clen);
        size_t mC = Cdim.getRows();
        size_t nC = Cdim.getColumns();
        Eigen::Map<Eigen::MatrixXf> matC((single*)Cp, mC, nC);
        Dimensions dimA = a.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = b.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (a.isScalar())
        {
            Eigen::Map<Eigen::MatrixXf> matB((single*)b.getDataPointer(), mB, nB);
            matC = a.getContentsAsSingleScalar() - matB.array();
        }
        else if (b.isScalar())
        {
            Eigen::Map<Eigen::MatrixXf> matA((single*)a.getDataPointer(), mA, nA);
            matC = matA.array() - b.getContentsAsSingleScalar();
        }
        else
        {
            Eigen::Map<Eigen::MatrixXf> matA((single*)a.getDataPointer(), mA, nA);
            Eigen::Map<Eigen::MatrixXf> matB((single*)b.getDataPointer(), mB, nB);
            matC = matA - matB;
        }
        return ArrayOf(NLS_SINGLE, Cdim, Cp, false);
    }
    //=============================================================================
    ArrayOf scomplex_subtraction(ArrayOf a, ArrayOf  b)
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
        void *Cp = new_with_exception<single>(Clen * 2);
        singlecomplex* Cz = reinterpret_cast<singlecomplex*>(Cp);
        size_t mC = Cdim.getRows();
        size_t nC = Cdim.getColumns();
        Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
        Dimensions dimA = a.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        Dimensions dimB = b.getDimensions();
        size_t mB = dimB.getRows();
        size_t nB = dimB.getColumns();
        if (a.isScalar())
        {
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)a.getDataPointer());
            if (b.getDataClass() == NLS_SCOMPLEX)
            {
                singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)b.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                matC = Az[0] - matB.array();
            }
            else
            {
                single *Bz = (single*)b.getDataPointer();
                Eigen::Map<Eigen::MatrixXf> matB(Bz, mB, nB);
                matC = Az[0] - matB.cast<singlecomplex>().array();
            }
        }
        else if (b.isScalar())
        {
            singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)b.getDataPointer());
            if (a.getDataClass() == NLS_SCOMPLEX)
            {
                singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)a.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                matC = matA.array() - Bz[0];
            }
            else
            {
                single *Az = (single*)a.getDataPointer();
                Eigen::Map<Eigen::MatrixXf> matA(Az, mA, nA);
                matC = matA.cast<singlecomplex>().array() - Bz[0];
            }
        }
        else
        {
            singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)a.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
            singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)b.getDataPointer());
            Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
            matC = matA - matB;
        }
        return ArrayOf(NLS_SCOMPLEX, Cdim, Cp, false);
    }
    //=============================================================================
    ArrayOf single_minus_single(ArrayOf a, ArrayOf b)
    {
        if (a.isComplex() || b.isComplex())
        {
            return scomplex_subtraction(a, b);
        }
        return single_subtraction(a, b);
    }
    //=============================================================================

}
//=============================================================================
