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
#include "IJVBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "SparseType.hpp"
#include "SparseNonZeros.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::SparseGateway::IJVBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 6)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, bSuccess);
    if (!bSuccess)
    {
        ArrayOf A(argIn[0]);
        if (A.isSparse())
        {
            Dimensions dims = A.getDimensions();
            indexType nnz = SparseNonZeros(A);
            indexType *ptrI;
            indexType *ptrJ;
            try
            {
                ptrI = new indexType[nnz];
                ptrJ = new indexType[nnz];
            }
            catch (std::bad_alloc &e)
            {
                e.what();
                throw Exception(ERROR_MEMORY_ALLOCATION);
            }
            int nz = 0;
            void *ptrV = Eigen_SparseToIJV(A.getDataClass(), dims.getRows(), dims.getColumns(), A.getSparseDataPointer(), ptrI, ptrJ, nz);
            double *pdI = (double *)ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz);
            for (indexType k = 0; k < nnz; k++)
            {
                pdI[k] = (double)ptrI[k];
            }
            ArrayOf I = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdI);
            retval.push_back(I);
            delete[] ptrI;
            if (nLhs > 1)
            {
                double *pdJ = (double *)ArrayOf::allocateArrayOf(NLS_DOUBLE, nnz);
                for (indexType k = 0; k < nnz; k++)
                {
                    pdJ[k] = (double)ptrJ[k];
                }
                ArrayOf J = ArrayOf(NLS_DOUBLE, Dimensions(nnz, 1), (void*)pdJ);
                retval.push_back(J);
            }
            delete[] ptrJ;
            if (nLhs > 2)
            {
                ArrayOf V = ArrayOf(A.getDataClass(), Dimensions(nnz, 1), ptrV);
                retval.push_back(V);
            }
            if (nLhs > 3)
            {
                ArrayOf M = ArrayOf::doubleConstructor((double)dims.getRows());
                retval.push_back(M);
            }
            if (nLhs > 4)
            {
                ArrayOf N = ArrayOf::doubleConstructor((double)dims.getColumns());
                retval.push_back(N);
            }
            if (nLhs > 5)
            {
                ArrayOf NNZ = ArrayOf::doubleConstructor((double)A.nzmax());
                retval.push_back(NNZ);
            }
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
