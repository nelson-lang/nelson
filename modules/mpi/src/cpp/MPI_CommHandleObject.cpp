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
#include "MPI_CommHandleObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    MPI_CommHandleObject::MPI_CommHandleObject(void *_ptr) : HandleGenericObject(std::wstring(MPI_COMM_CATEGORY_STR), _ptr)
    {
    }
    //=============================================================================
    MPI_CommHandleObject::~MPI_CommHandleObject()
    {
    }
    //=============================================================================
    MPI_Comm HandleToMpiComm(ArrayOf A)
    {
        MPI_Comm commReturned = MPI_COMM_NULL;
        if (A.isHandle())
        {
            if (!A.isScalar())
            {
                throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
            }
            nelson_handle *qp = (nelson_handle*)A.getDataPointer();
            if (qp == nullptr)
            {
                throw Exception(_W("MPI_Comm valid handle expected."));
            }
            nelson_handle hl = qp[0];
            HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj == nullptr)
            {
                throw Exception(_W("MPI_Comm valid handle expected."));
            }
            if (hlObj->getCategory() != MPI_COMM_CATEGORY_STR)
            {
                throw Exception(_W("MPI_Comm handle expected."));
            }
            MPI_CommObject *mpicommhandleobj = (MPI_CommObject *)hlObj;
            commReturned = mpicommhandleobj->getComm();
        }
        else
        {
            throw Exception(_W("MPI_Comm handle expected."));
        }
        return commReturned;
    }
    //=============================================================================
    ArrayOf MpiCommToHandle(MPI_Comm mpicomm, std::wstring description)
    {
        return ArrayOf::handleConstructor(new MPI_CommHandleObject(new MPI_CommObject(description, MPI_COMM_WORLD)));
    }
    //=============================================================================
}
//=============================================================================
