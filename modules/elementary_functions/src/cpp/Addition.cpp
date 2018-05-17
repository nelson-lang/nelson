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
#include <Eigen/Dense>
#include "Addition.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static ArrayOf double_plus_double(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess);
    static ArrayOf dcomplex_plus_dcomplex(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess);
    static ArrayOf single_plus_single(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess);
    static ArrayOf scomplex_plus_scomplex(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess);
    static ArrayOf empty_plus_generic(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess);
    static Dimensions getOutputDimensions(ArrayOf A, ArrayOf B);
    //=============================================================================
    ArrayOf Addition(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        bSuccess = false;
        if (A.isDoubleType(true) && B.isDoubleType(true))
        {
            return double_plus_double(A, B, mustRaiseError, bSuccess);
        }
        if (A.isSingleType(true) && B.isSingleType(true))
        {
            return single_plus_single(A, B, mustRaiseError, bSuccess);
        }
        if (A.isDoubleType(false) && B.isDoubleType(false))
        {
            if (A.getDataClass() == B.getDataClass())
            {
                return dcomplex_plus_dcomplex(A, B, mustRaiseError, bSuccess);
            }
            else
            {
                A.promoteType(NLS_DCOMPLEX);
                B.promoteType(NLS_DCOMPLEX);
                return dcomplex_plus_dcomplex(A, B, mustRaiseError, bSuccess);
            }
        }
        if (A.isSingleType(false) && B.isSingleType(false))
        {
            if (A.getDataClass() == B.getDataClass())
            {
                return scomplex_plus_scomplex(A, B, mustRaiseError, bSuccess);
            }
            else
            {
                A.promoteType(NLS_SCOMPLEX);
                B.promoteType(NLS_SCOMPLEX);
                return scomplex_plus_scomplex(A, B, mustRaiseError, bSuccess);
            }
        }
        if (mustRaiseError)
        {
            std::string overload = ClassName(A) + "_plus_" + ClassName(B);
            throw Exception(_("function") + " " + overload + " " + _("undefined."));
        }
        return ArrayOf();
    }
    //=============================================================================
    ArrayOf empty_plus_generic(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        ArrayOf res;
        Dimensions dimA = A.getDimensions();
        size_t mA = dimA.getRows();
        size_t nA = dimA.getColumns();
        if (mA == nA)
        {
            if (B.isEmpty())
            {
                Dimensions dimB = B.getDimensions();
                size_t mB = dimB.getRows();
                size_t nB = dimB.getColumns();
                if ((mB == mA) && (nA == nB))
                {
                    bSuccess = true;
                    return ArrayOf(A);
                }
                else
                {
                    if (mustRaiseError)
                    {
                        throw Exception(_W("using operator '+' \n Matrix dimensions must agree."));
                    }
                    else
                    {
                        bSuccess = false;
                        return ArrayOf();
                    }
                }
            }
            if (B.isScalar())
            {
                // [] + X returns []
                bSuccess = true;
                return ArrayOf(A);
            }
            else
            {
                if (mustRaiseError)
                {
                    throw Exception(_W("using operator '+' \n Matrix dimensions must agree."));
                }
                else
                {
                    bSuccess = false;
                    return ArrayOf();
                }
            }
        }
        return res;
    }
    //=============================================================================
    Dimensions getOutputDimensions(ArrayOf A, ArrayOf B)
    {
        Dimensions outputDimensions;
        if (A.isScalar())
        {
            outputDimensions = B.getDimensions();
        }
        else
        {
            outputDimensions = A.getDimensions();
        }
        return outputDimensions;
    }
    //=============================================================================
    ArrayOf double_plus_double(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        ArrayOf res;
        if (A.isEmpty())
        {
            return empty_plus_generic(A, B, mustRaiseError, bSuccess);
        }
        if (A.isScalar() && B.isScalar())
        {
            bSuccess = true;
            double *ptrA = (double*)A.getDataPointer();
            double *ptrB = (double*)B.getDataPointer();
            res = ArrayOf::doubleConstructor(ptrA[0] + ptrB[0]);
        }
        else
        {
            Dimensions dimsC = getOutputDimensions(A, B);
            indexType Clen = dimsC.getElementCount();
            void *Cp = new_with_exception<double>(Clen);
            size_t mC = dimsC.getRows();
            size_t nC = dimsC.getColumns();
            Eigen::Map<Eigen::MatrixXd> matC((double*)Cp, mC, nC);
            Dimensions dimA = A.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            Dimensions dimB = B.getDimensions();
            size_t mB = dimB.getRows();
            size_t nB = dimB.getColumns();
            if (A.isScalar())
            {
                Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                matC = A.getContentAsDoubleScalar() + matB.array();
            }
            else if (B.isScalar())
            {
                Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                matC = matA.array() + B.getContentAsDoubleScalar();
            }
            else
            {
                Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), mA, nA);
                Eigen::Map<Eigen::MatrixXd> matB((double*)B.getDataPointer(), mB, nB);
                matC = matA + matB;
            }
            res = ArrayOf(NLS_DOUBLE, dimsC, Cp, false);
            bSuccess = true;
        }
        return res;
    }
    //=============================================================================
    ArrayOf dcomplex_plus_dcomplex(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        ArrayOf res;
        if (A.isEmpty())
        {
            return empty_plus_generic(A, B, mustRaiseError, bSuccess);
        }
        if (A.isScalar() && B.isScalar())
        {
            res = A;
            res.ensureSingleOwner();
            double *da = (double*)A.getDataPointer();
            double *db = (double*)B.getDataPointer();
            double *dres = (double*)res.getDataPointer();
            dres[0] = da[0] + db[0];
            dres[1] = da[1] + db[1];
            bSuccess = true;
        }
        else
        {
            Dimensions dimsC = getOutputDimensions(A, B);
            indexType Clen = dimsC.getElementCount();
            void *Cp = new_with_exception<double>(Clen * 2);
            doublecomplex* Cz = reinterpret_cast<doublecomplex*>(Cp);
            size_t mC = dimsC.getRows();
            size_t nC = dimsC.getColumns();
            Eigen::Map<Eigen::MatrixXcd> matC(Cz, mC, nC);
            Dimensions dimA = A.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            Dimensions dimB = B.getDimensions();
            size_t mB = dimB.getRows();
            size_t nB = dimB.getColumns();
            if (A.isScalar())
            {
                double *da = (double*)A.getDataPointer();
                doublecomplex* Az = reinterpret_cast<doublecomplex*>(da);
                if (B.getDataClass() == NLS_DCOMPLEX)
                {
                    double *db = (double*)B.getDataPointer();
                    doublecomplex* Bz = reinterpret_cast<doublecomplex*>(db);
                    Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                    matC = Az[0] + matB.array();
                }
                else
                {
                    double *Bz = (double*)B.getDataPointer();
                    Eigen::Map<Eigen::MatrixXd> matB(Bz, mB, nB);
                    matC = Az[0] + matB.cast<doublecomplex>().array();
                }
            }
            else if (B.isScalar())
            {
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)B.getDataPointer());
                if (A.getDataClass() == NLS_DCOMPLEX)
                {
                    doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
                    Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                    matC = matA.array() + Bz[0];
                }
                else
                {
                    double *Az = (double*)A.getDataPointer();
                    Eigen::Map<Eigen::MatrixXd> matA(Az, mA, nA);
                    matC = matA.cast<doublecomplex>().array() + Bz[0];
                }
            }
            else
            {
                doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matA(Az, mA, nA);
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>((double*)B.getDataPointer());
                Eigen::Map<Eigen::MatrixXcd> matB(Bz, mB, nB);
                matC = matA + matB;
            }
            res = ArrayOf(NLS_DCOMPLEX, dimsC, Cp, false);
        }
        if (res.allReal())
        {
            res.promoteType(NLS_DOUBLE);
        }
        bSuccess = true;
        return res;
    }
    //=============================================================================
    ArrayOf single_plus_single(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        ArrayOf res;
        if (A.isEmpty())
        {
            return empty_plus_generic(A, B, mustRaiseError, bSuccess);
        }
        if (A.isScalar() && B.isScalar())
        {
            single *ptrA = (single*)A.getDataPointer();
            single *ptrB = (single*)A.getDataPointer();
            res = ArrayOf::singleConstructor(ptrA[0] + ptrB[0]);
        }
        else
        {
            Dimensions dimsC = getOutputDimensions(A, B);
            indexType Clen = dimsC.getElementCount();
            void *Cp = new_with_exception<single>(Clen);
            size_t mC = dimsC.getRows();
            size_t nC = dimsC.getColumns();
            Eigen::Map<Eigen::MatrixXf> matC((single*)Cp, mC, nC);
            Dimensions dimA = A.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            Dimensions dimB = B.getDimensions();
            size_t mB = dimB.getRows();
            size_t nB = dimB.getColumns();
            if (A.isScalar())
            {
                Eigen::Map<Eigen::MatrixXf> matB((single*)B.getDataPointer(), mB, nB);
                matC = A.getContentAsSingleScalar() + matB.array();
            }
            else if (B.isScalar())
            {
                Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), mA, nA);
                matC = matA.array() + B.getContentAsSingleScalar();
            }
            else
            {
                Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), mA, nA);
                Eigen::Map<Eigen::MatrixXf> matB((single*)B.getDataPointer(), mB, nB);
                matC = matA + matB;
            }
            res = ArrayOf(NLS_SINGLE, dimsC, Cp, false);
        }
        bSuccess = true;
        return res;
    }
    //=============================================================================
    ArrayOf scomplex_plus_scomplex(ArrayOf A, ArrayOf B, bool mustRaiseError, bool &bSuccess)
    {
        ArrayOf res;
        if (A.isEmpty())
        {
            return empty_plus_generic(A, B, mustRaiseError, bSuccess);
        }
        if (A.isScalar() && B.isScalar())
        {
            res = A;
            res.ensureSingleOwner();
            single *da = (single*)A.getDataPointer();
            single *db = (single*)B.getDataPointer();
            single *dres = (single*)res.getDataPointer();
            dres[0] = da[0] + db[0];
            dres[1] = da[1] + db[1];
        }
        else
        {
            Dimensions dimsC = getOutputDimensions(A, B);
            indexType Clen = dimsC.getElementCount();
            void *Cp = new_with_exception<single>(Clen * 2);
            singlecomplex* Cz = reinterpret_cast<singlecomplex*>(Cp);
            size_t mC = dimsC.getRows();
            size_t nC = dimsC.getColumns();
            Eigen::Map<Eigen::MatrixXcf> matC(Cz, mC, nC);
            Dimensions dimA = A.getDimensions();
            size_t mA = dimA.getRows();
            size_t nA = dimA.getColumns();
            Dimensions dimB = B.getDimensions();
            size_t mB = dimB.getRows();
            size_t nB = dimB.getColumns();
            if (A.isScalar())
            {
                singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                if (B.getDataClass() == NLS_SCOMPLEX)
                {
                    singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)B.getDataPointer());
                    Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                    matC = Az[0] + matB.array();
                }
                else
                {
                    single *Bz = (single*)B.getDataPointer();
                    Eigen::Map<Eigen::MatrixXf> matB(Bz, mB, nB);
                    matC = Az[0] + matB.cast<singlecomplex>().array();
                }
            }
            else if (B.isScalar())
            {
                singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)B.getDataPointer());
                if (A.getDataClass() == NLS_SCOMPLEX)
                {
                    singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                    Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                    matC = matA.array() + Bz[0];
                }
                else
                {
                    single *Az = (single*)A.getDataPointer();
                    Eigen::Map<Eigen::MatrixXf> matA(Az, mA, nA);
                    matC = matA.cast<singlecomplex>().array() + Bz[0];
                }
            }
            else
            {
                singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matA(Az, mA, nA);
                singlecomplex* Bz = reinterpret_cast<singlecomplex*>((single*)B.getDataPointer());
                Eigen::Map<Eigen::MatrixXcf> matB(Bz, mB, nB);
                matC = matA + matB;
            }
            res = ArrayOf(NLS_SCOMPLEX, dimsC, Cp, false);
        }
        if (res.allReal())
        {
            res.promoteType(NLS_SINGLE);
        }
        bSuccess = true;
        return res;
    }
    //=============================================================================
}
//=============================================================================
