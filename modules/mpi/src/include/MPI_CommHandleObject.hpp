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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "HandleGenericObject.hpp"
#include "nlsMpi_exports.h"
#include <mpi.h>
#include <string>
//=============================================================================
#define MPI_COMM_CATEGORY_STR L"MPI_Comm"
//=============================================================================
namespace Nelson {
//=============================================================================
class MPI_CommObject
{
private:
    MPI_Comm comm;

public:
    MPI_CommObject(MPI_Comm _comm) { comm = _comm; }
    ~MPI_CommObject() {}
    MPI_Comm
    getComm()
    {
        return comm;
    }
};
//=============================================================================
class NLSMPI_IMPEXP MPI_CommHandleObject : public HandleGenericObject
{
public:
    MPI_CommHandleObject(void* commPtr);
    ~MPI_CommHandleObject();
};
//=============================================================================
NLSMPI_IMPEXP MPI_Comm
HandleToMpiComm(ArrayOf A);
NLSMPI_IMPEXP ArrayOf
MpiCommToHandle(MPI_Comm mpicomm);
NLSMPI_IMPEXP bool
MPICommHandleDelete(ArrayOf A);
//=============================================================================
} // namespace Nelson
//=============================================================================
