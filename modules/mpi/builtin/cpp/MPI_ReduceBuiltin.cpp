//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_ReduceBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
// x = MPI_Reduce(A, operation, root, comm)
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_ReduceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf A = argIn[0];
    if (A.isSparse() || A.isReferenceType()) {
        Error(_W("Unsupported type to reduce."));
    }
    ArrayOf Operation = argIn[1];
    std::wstring op_str = Operation.getContentAsWideString();
    MPI_Op mpi_op = stringToMpiOp(op_str);
    if (mpi_op == MPI_OP_NULL) {
        Error(_W("Unsupported operator type."));
    }
    ArrayOf Root = argIn[2];
    int rootID = Root.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 3) {
        comm = HandleToMpiComm(argIn[3]);
    }
    ArrayOf dest = A;
    dest.ensureSingleOwner();
    NelsonType dataClass = A.getDataClass();
    switch (dataClass) {
    case NLS_LOGICAL:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_UINT8_T, mpi_op, rootID, comm);
        break;
    case NLS_UINT8:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_UINT8_T, mpi_op, rootID, comm);
        break;
    case NLS_INT8:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_INT8_T, mpi_op, rootID, comm);
        break;
    case NLS_UINT16:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_UNSIGNED_SHORT, mpi_op, rootID, comm);
        break;
    case NLS_INT16:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_SHORT, mpi_op, rootID, comm);
        break;
    case NLS_UINT32:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_UINT32_T, mpi_op, rootID, comm);
        break;
    case NLS_INT32:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_INT32_T, mpi_op, rootID, comm);
        break;
    case NLS_UINT64:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_UINT64_T, mpi_op, rootID, comm);
        break;
    case NLS_INT64:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_INT64_T, mpi_op, rootID, comm);
        break;
    case NLS_SINGLE:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_FLOAT, mpi_op, rootID, comm);
        break;
    case NLS_DOUBLE:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            (int)A.getElementCount(), MPI_DOUBLE, mpi_op, rootID, comm);
        break;
    case NLS_SCOMPLEX:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            2 * (int)A.getElementCount(), MPI_FLOAT, mpi_op, rootID, comm);
        break;
    case NLS_DCOMPLEX:
        MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(),
            2 * (int)A.getElementCount(), MPI_DOUBLE, mpi_op, rootID, comm);
        break;
    default:
        Error(_W("Unsupported Type: must be a numerical type."));
    }
    retval << dest;
    return retval;
}
//=============================================================================
