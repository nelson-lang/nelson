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
#include "ArrayOf.hpp"
#include "Data.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    /**
    * Returns TRUE if it is a single type (not ndarray, not sparse)
    */
    const bool ArrayOf::isSingleType() const
    {
		return (!dp->sparse) && is2D() && ((dp->dataClass == NLS_SINGLE) || (dp->dataClass == NLS_SCOMPLEX));
    }
    //=============================================================================
    const bool ArrayOf::isNdArraySingleType() const
    {
		return (!dp->sparse) && !is2D() && ((dp->dataClass == NLS_SINGLE) || (dp->dataClass == NLS_SCOMPLEX));
    }
    //=============================================================================
    ArrayOf ArrayOf::singleConstructor(float aval)
    {
        Dimensions dim;
        dim.makeScalar();
        float *data = (float *)allocateArrayOf(NLS_SINGLE, 1);
        *data = aval;
        return ArrayOf(NLS_SINGLE, dim, data);
    }
    //=============================================================================
    ArrayOf ArrayOf::complexConstructor(float aval, float bval)
    {
        Dimensions dim;
        dim.makeScalar();
        float *data = (float *)allocateArrayOf(NLS_SCOMPLEX, 1);
        data[0] = aval;
        data[1] = bval;
        return ArrayOf(NLS_SCOMPLEX, dim, data);
    }
    //=============================================================================
    single ArrayOf::getContentAsSingleScalar()
    {
        single *qp;
        if (isComplex() || isReferenceType() || isString())
        {
            throw Exception(_W("Expected a real valued scalar"));
        }
        promoteType(NLS_SINGLE);
        qp = (single*)dp->getData();
        return (*qp);
    }
    //=============================================================================
    std::complex<single> ArrayOf::getContentAsSingleComplexScalar()
    {
        if (isReferenceType() || isString())
        {
            throw Exception(_W("Expected a real valued scalar"));
        }
        promoteType(NLS_SCOMPLEX);
        single *qp = (single*)dp->getData();
        std::complex<single> cx(qp[0], qp[1]);
        return cx;
    }
    //=============================================================================

}
//=============================================================================
