//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
namespace Nelson {
//=============================================================================
class MPI_CommObject
{
private:
    MPI_Comm comm;

public:
    MPI_CommObject(MPI_Comm _comm) : comm(_comm) { }
    ~MPI_CommObject() = default;
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
    MPI_CommHandleObject(void* _ptr);
    ~MPI_CommHandleObject() override;
};
//=============================================================================
NLSMPI_IMPEXP MPI_Comm
HandleToMpiComm(const ArrayOf& A);
NLSMPI_IMPEXP ArrayOf
MpiCommToHandle(MPI_Comm mpicomm);
NLSMPI_IMPEXP bool
MPICommHandleDelete(const ArrayOf& A);
//=============================================================================
} // namespace Nelson
//=============================================================================
