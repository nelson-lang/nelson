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
#include <mpi.h>
#include "MPI_Comm_dispBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Comm_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (eval != nullptr)
    {
        Interface *io = eval->getInterface();
        if (io)
        {
            if (param1.isHandle())
            {
                if (param1.isScalar())
                {
                    nelson_handle *qp = (nelson_handle*)param1.getDataPointer();
                    nelson_handle hl = qp[0];
                    HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
                    if (hlObj->getCategory() != MPI_COMM_CATEGORY_STR)
                    {
                        throw Exception(_W("MPI_Comm handle expected."));
                    }
                    Dimensions dimsParam1 = param1.getDimensions();
                    io->outputMessage(L"[MPI_Comm] - size: ");
                    dimsParam1.printMe(io);
                    io->outputMessage("\n");
                    io->outputMessage("\n");
                    MPI_CommHandleObject *mpicommhandleobj = (MPI_CommHandleObject *)hlObj;
                    if (mpicommhandleobj != nullptr)
                    {
                        MPI_CommObject *obj = (MPI_CommObject *)mpicommhandleobj->getPointer();
                        if (obj != nullptr)
                        {
                            io->outputMessage(L"    " + _W("Description") + L":    " + obj->getDescription());
                            io->outputMessage("\n");
                        }
                    }
                }
                else
                {
                    Dimensions dimsParam1 = param1.getDimensions();
                    io->outputMessage(L"[MPI_Comm] - size: ");
                    dimsParam1.printMe(io);
                    io->outputMessage("\n");
                }
            }
            else
            {
                Error(eval, _W("MPI_Comm handle expected."));
            }
        }
    }
    return retval;
}
//=============================================================================
