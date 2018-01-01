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
#include "MPI_Comm_isvalidBuiltin.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Comm_isvalidBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (param1.isHandle())
    {
        Dimensions dimsparam1 = param1.getDimensions();
        nelson_handle *qp = (nelson_handle*)param1.getDataPointer();
        if (qp)
        {
            logical *resArray = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsparam1.getElementCount());
            for (size_t k = 0; k < dimsparam1.getElementCount(); k++)
            {
                nelson_handle hl = qp[k];
                HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr)
                {
                    if (hlObj->getPointer())
                    {
                        resArray[k] = true;
                    }
                    else
                    {
                        resArray[k] = false;
                    }
                }
                else
                {
                    resArray[k] = false;
                }
            }
            retval.push_back(ArrayOf(NLS_LOGICAL, dimsparam1, resArray));
        }
        else
        {
            retval.push_back(ArrayOf::emptyConstructor(dimsparam1));
        }
    }
    else
    {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
