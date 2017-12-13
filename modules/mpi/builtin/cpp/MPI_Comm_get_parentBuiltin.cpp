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
#include "MPI_Comm_get_parentBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Comm_get_parentBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit)
    {
        Error(eval, _W("MPI must be initialized."));
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    MPI_Comm parent;
    int res = MPI_Comm_get_parent(&parent);
    if (parent == MPI_COMM_NULL)
    {
        Error(eval, _W("MPI_Comm: Invalid communicator."));
    }
    std::vector<nelson_handle> hdl_comms = HandleManager::getInstance()->getAllHandlesOfCategory(MPI_COMM_CATEGORY_STR);
    bool found = false;
    for (size_t k = 0; k < hdl_comms.size(); ++k)
    {
        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hdl_comms[k]);
        if (hlObj != nullptr)
        {
            int result = MPI_UNEQUAL;
            MPI_CommObject *mpicommhandleobj = (MPI_CommObject *)hlObj;
            MPI_Comm_compare(parent, mpicommhandleobj->getComm(), &result);
            found = (result == MPI_IDENT);
            if (found)
            {
                retval.push_back(ArrayOf::handleConstructor(hdl_comms[k]));
                return retval;
            }
        }
    }
    retval.push_back(MpiCommToHandle(parent));
    return retval;
}
//=============================================================================
