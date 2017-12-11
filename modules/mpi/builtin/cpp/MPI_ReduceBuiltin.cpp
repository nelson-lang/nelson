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
#include "MPI_ReduceBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// x = MPI_Reduce(A, operation, root, comm)
//=============================================================================
static MPI_Op stringToMpiOp(std::wstring op_str)
{
    MPI_Op mpi_op = MPI_OP_NULL;
    if (op_str == L"MPI_SUM")
    {
        return MPI_SUM;
    }
    if (op_str == L"MPI_MAX")
    {
        return MPI_MAX;
    }
    if (op_str == L"MPI_MIN")
    {
        return MPI_MIN;
    }
    if (op_str == L"MPI_SUM")
    {
        return MPI_SUM;
    }
    if (op_str == L"MPI_PROD")
    {
        return MPI_PROD;
    }
    if (op_str == L"MPI_LAND")
    {
        return MPI_LAND;
    }
    if (op_str == L"MPI_LOR")
    {
        return MPI_LOR;
    }
    if (op_str == L"MPI_BAND")
    {
        return MPI_BAND;
    }
    if (op_str == L"MPI_BOR")
    {
        return MPI_BOR;
    }
    if (op_str == L"MPI_LXOR")
    {
        return MPI_LXOR;
    }
    if (op_str == L"MPI_BXOR")
    {
        return MPI_BXOR;
    }
    return mpi_op;
}
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_ReduceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() < 3) || (argIn.size() > 4))
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit)
    {
        Error(eval, _W("MPI must be initialized."));
    }
    ArrayOf A = argIn[0];
    if (A.isSparse() || A.isReferenceType())
    {
        Error(eval, _W("Unsupported type to reduce."));
    }
    ArrayOf Operation = argIn[1];
    std::wstring op_str = Operation.getContentAsWideString();
    MPI_Op mpi_op = stringToMpiOp(op_str);
    if (mpi_op == MPI_OP_NULL)
    {
        Error(eval, _W("Unsupported operator type."));
    }
    ArrayOf Root = argIn[2];
    int rootID = Root.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 3)
    {
        comm = HandleToMpiComm(argIn[3]);
    }
    ArrayOf dest = A;
    dest.ensureSingleOwner();
    Class dataClass = A.getDataClass();
    switch (dataClass)
    {
        case NLS_LOGICAL:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_UINT8_T, mpi_op, rootID, comm);
            break;
        case NLS_UINT8:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_UINT8_T, mpi_op, rootID, comm);
            break;
        case NLS_INT8:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_INT8_T, mpi_op, rootID, comm);
            break;
        case NLS_UINT16:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_UNSIGNED_SHORT, mpi_op, rootID, comm);
            break;
        case NLS_INT16:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_SHORT, mpi_op, rootID, comm);
            break;
        case NLS_UINT32:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_UINT32_T, mpi_op, rootID, comm);
            break;
        case NLS_INT32:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_INT32_T, mpi_op, rootID, comm);
            break;
        case NLS_UINT64:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_UINT64_T, mpi_op, rootID, comm);
            break;
        case NLS_INT64:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_INT64_T, mpi_op, rootID, comm);
            break;
        case NLS_SINGLE:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_FLOAT, mpi_op, rootID, comm);
            break;
        case NLS_DOUBLE:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), (int)A.getLength(), MPI_DOUBLE, mpi_op, rootID, comm);
            break;
        case NLS_SCOMPLEX:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), 2 * (int)A.getLength(), MPI_FLOAT, mpi_op, rootID, comm);
            break;
        case NLS_DCOMPLEX:
            MPI_Reduce((void*)A.getDataPointer(), dest.getReadWriteDataPointer(), 2 * (int)A.getLength(), MPI_DOUBLE, mpi_op, rootID, comm);
            break;
        default:
            Error(eval, _W("Unsupported Type: must be a numerical type."));
    }
    retval.push_back(dest);
    return retval;
}
//=============================================================================
