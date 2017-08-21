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
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void CheckNumeric(ArrayOf &A, ArrayOf &B, const std::string &opname) throw(Exception)
    {
        bool Anumeric, Bnumeric;
        Anumeric = !A.isReferenceType();
        Bnumeric = !B.isReferenceType();
        if (!(Anumeric && Bnumeric))
            throw Exception(std::string(_("Cannot apply numeric operation ")) +
                            opname + std::string(_(" to reference types.")));
    }
    //=============================================================================
    bool MatrixCheck(ArrayOf &A, ArrayOf &B, const std::string &opname) throw(Exception)
    {
        // Test for either a scalar (test 1)
        if (A.isScalar() || B.isScalar())
        {
            return false;
        }
        // Test for A & B numeric
        CheckNumeric(A, B, opname);
        // Test for 2D
        if (!A.is2D() || !B.is2D())
            throw Exception(std::string(_("Cannot apply matrix operation ")) +
                            opname + std::string(_(" to N-Dimensional arrays.")));
        // Test the types
        TypeCheck(A, B, true);
        return true;
    }
    //=============================================================================
    void TypeCheck(ArrayOf &A, ArrayOf &B, bool isDivOrMatrix)
    {
        Class Aclass, Bclass, Cclass;
        Aclass = A.getDataClass();
        Bclass = B.getDataClass();
        if ( (Aclass == Bclass) &&
                ( (Aclass == NLS_LOGICAL) ||
                  (Aclass == NLS_UINT8) ||
                  (Aclass == NLS_INT8) ||
                  (Aclass == NLS_UINT16) ||
                  (Aclass == NLS_INT16) ||
                  (Aclass == NLS_UINT32) ||
                  (Aclass == NLS_INT32) ||
                  (Aclass == NLS_UINT64) ||
                  (Aclass == NLS_INT64) ||
                  (Aclass == NLS_SINGLE) ||
                  (Aclass == NLS_DOUBLE) ||
                  (Aclass == NLS_SCOMPLEX) ||
                  (Aclass == NLS_DCOMPLEX) ||
                  (Aclass == NLS_CHAR)) )
        {
            return;
        }
        if ((Aclass < NLS_INT32) || (Aclass == NLS_CHAR))
        {
            Aclass = NLS_INT32;
        }
        if ((Bclass < NLS_INT32) || (Bclass == NLS_CHAR))
        {
            Bclass = NLS_INT32;
        }
        // Division or matrix operations do no allow integer
        // data types.  These must be promoted to doubles.
        if (isDivOrMatrix && (Aclass < NLS_SINGLE))
        {
            Aclass = NLS_DOUBLE;
        }
        if (isDivOrMatrix && (Bclass < NLS_SINGLE))
        {
            Bclass = NLS_DOUBLE;
        }
        // An integer or double mixed with a complex is promoted to a dcomplex type
        if ((Aclass == NLS_SCOMPLEX) && ((Bclass == NLS_DOUBLE) || (Bclass < NLS_SINGLE)))
        {
            Bclass = NLS_DCOMPLEX;
        }
        if ((Bclass == NLS_SCOMPLEX) && ((Aclass == NLS_DOUBLE) || (Aclass < NLS_SINGLE)))
        {
            Aclass = NLS_DCOMPLEX;
        }
        // The output class is now the dominant class remaining:
        Cclass = (Aclass > Bclass) ? Aclass : Bclass;
        A.promoteType(Cclass);
        B.promoteType(Cclass);
    }
    //=============================================================================
    bool SameSizeCheck(Dimensions Adim, Dimensions Bdim)
    {
        Adim.simplify();
        Bdim.simplify();
        return (Adim.equals(Bdim));
    }
    //=============================================================================
    void VectorCheck(ArrayOf& A, ArrayOf& B, bool promote, const std::string &opname) throw(Exception)
    {
        stringVector dummySV;
        // Check for numeric types
        CheckNumeric(A, B, opname);
        if (!(SameSizeCheck(A.getDimensions(), B.getDimensions()) || A.isScalar() || B.isScalar()))
        {
            throw Exception(std::string(_("Size mismatch on arguments to arithmetic operator ")) + opname);
        }
        // Test the types.
        TypeCheck(A, B, promote);
    }
    //=============================================================================
    void BoolVectorCheck(ArrayOf& A, ArrayOf& B, const std::string &opname) throw(Exception)
    {
        A.promoteType(NLS_LOGICAL);
        B.promoteType(NLS_LOGICAL);
        if (!(SameSizeCheck(A.getDimensions(), B.getDimensions()) || A.isScalar() || B.isScalar()))
        {
            throw Exception(std::string(_("Size mismatch on arguments to ")) + opname);
        }
    }
    //=============================================================================

}
//=============================================================================
