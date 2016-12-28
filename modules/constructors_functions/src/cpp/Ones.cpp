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
#include "Ones.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    ArrayOf Ones(Evaluator *eval, Class cl)
    {
        Dimensions dims(1, 1);
        return Ones(eval, dims, cl);
    }
    //=============================================================================
    ArrayOf Ones(Evaluator *eval, Dimensions dims, Class cl)
    {
        switch (cl)
        {
            case NLS_SINGLE:
            {
                indexType nbElements = dims.getElementCount();
                single * mat = (single*)ArrayOf::allocateArrayOf(cl, nbElements);
                if (((int)nbElements % 2) == 0)
                {
                    for (indexType k = 0; k < nbElements; k = k + 2)
                    {
                        mat[k] = (single)1.0;
                        mat[k + 1] = (single)1.0;
                    }
                }
                else
                {
                    for (indexType k = 0; k < nbElements; k++)
                    {
                        mat[k] = (single)1.0;
                    }
                }
                return ArrayOf(cl, dims, mat, false);
            }
            break;
            case NLS_DOUBLE:
            {
                indexType nbElements = dims.getElementCount();
                double * mat = (double*)ArrayOf::allocateArrayOf(cl, nbElements);
                /*if (((int)nbElements % 2) == 0)
                {
                    for (indexType k = 0; k < nbElements; k = k + 2)
                    {
                        mat[k] = (double)1.0;
                        mat[k+1] = (double)1.0;
                    }
                }
                else
                {
                    for (indexType k = 0; k < nbElements; k++)
                    {
                        mat[k] = (double)1.0;
                    }
                }*/
                indexType k = 0;
                while(k < nbElements)
                {
                    mat[k++] = (double)1.0;
                }
                return ArrayOf(cl, dims, mat, false);
            }
            break;
            default:
                Error(eval, ERROR_TYPE_NOT_SUPPORTED);
        }
        return ArrayOf();
    }
    //=============================================================================
}
//=============================================================================
