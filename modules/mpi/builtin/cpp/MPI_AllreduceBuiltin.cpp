//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mpi.h>
#include "MPI_AllreduceBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// x = MPI_Allreduce(A, operation, comm)
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_AllreduceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(L"Nelson:MPI:notInitialized", L"MPI must be initialized.");
    }
    ArrayOf A = argIn[0];
    if (A.isSparse() || A.isReferenceType()) {
        Error(L"Nelson:MPI:typeNotSupported", L"Type not managed.");
    }
    ArrayOf Operation = argIn[1];
    std::wstring op_str = Operation.asWideString();
    MPI_Op mpi_op = stringToMpiOp(op_str);
    if (mpi_op == MPI_OP_NULL) {
        Error(L"Nelson:MPI:operatorNotSupported", L"Unsupported operator type.");
    }
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 2) {
        comm = HandleToMpiComm(argIn[2]);
    }
    ArrayOf dest = A;
    dest.ensureSingleOwner();
    NelsonType dataClass = A.dataClass();
    switch (dataClass) {
    case NLS_LOGICAL:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_UINT8_T, mpi_op, comm);
        break;
    case NLS_UINT8:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_UINT8_T, mpi_op, comm);
        break;
    case NLS_INT8:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_INT8_T, mpi_op, comm);
        break;
    case NLS_UINT16:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_UNSIGNED_SHORT, mpi_op, comm);
        break;
    case NLS_INT16:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_SHORT, mpi_op, comm);
        break;
    case NLS_UINT32:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_UINT32_T, mpi_op, comm);
        break;
    case NLS_INT32:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_INT32_T, mpi_op, comm);
        break;
    case NLS_UINT64:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_UINT64_T, mpi_op, comm);
        break;
    case NLS_INT64:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_INT64_T, mpi_op, comm);
        break;
    case NLS_SINGLE:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_FLOAT, mpi_op, comm);
        break;
    case NLS_DOUBLE:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(), (int)A.elementCount(),
            MPI_DOUBLE, mpi_op, comm);
        break;
    case NLS_SCOMPLEX:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(),
            2 * (int)A.elementCount(), MPI_FLOAT, mpi_op, comm);
        break;
    case NLS_DCOMPLEX:
        MPI_Allreduce((void*)A.dataPointer(), dest.readWriteDataPointer(),
            2 * (int)A.elementCount(), MPI_DOUBLE, mpi_op, comm);
        break;
    default:
        Error(L"Nelson:MPI:typeNotNumeric", L"Unsupported Type: must be a numerical type.");
    }
    retval << dest;
    return retval;
}
//=============================================================================
