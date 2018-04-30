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
#include <mpi.h>
#include "MPI_Comm_dispBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "MPI_helpers.hpp"
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
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit)
    {
        Error(eval, _W("MPI must be initialized."));
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
                    if (param1.getHandleCategory() != MPI_COMM_CATEGORY_STR)
                    {
                        throw Exception(_W("MPI_Comm handle expected."));
                    }
                    Dimensions dimsParam1 = param1.getDimensions();
                    io->outputMessage(L"[MPI_Comm] - size: ");
                    dimsParam1.printMe(io);
                    io->outputMessage("\n");
                    io->outputMessage("\n");
                    MPI_CommHandleObject *mpicommhandleobj = (MPI_CommHandleObject *)param1.getContentAsHandleScalar();
                    if (mpicommhandleobj != nullptr)
                    {
                        MPI_CommObject *obj = (MPI_CommObject *)mpicommhandleobj->getPointer();
                        if (obj != nullptr)
                        {
                            std::wstring description = utf8_to_wstring(getMpiCommName(obj->getComm()));
                            io->outputMessage(L"    " + _W("Description") + L":    " + description);
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
