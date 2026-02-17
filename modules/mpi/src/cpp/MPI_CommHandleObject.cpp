//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_CommHandleObject.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
MPI_CommHandleObject::MPI_CommHandleObject(void* _ptr)
    : HandleGenericObject(NLS_HANDLE_MPI_COMM_CATEGORY_STR, _ptr, false)
{
}
//=============================================================================
MPI_CommHandleObject::~MPI_CommHandleObject() = default;
//=============================================================================
MPI_Comm
HandleToMpiComm(const ArrayOf& A)
{
    MPI_Comm commReturned = MPI_COMM_NULL;
    if (A.getHandleCategory() != NLS_HANDLE_MPI_COMM_CATEGORY_STR) {
        raiseError(L"Nelson:mpi:ERROR_MPI_COMM_HANDLE_EXPECTED", ERROR_MPI_COMM_HANDLE_EXPECTED);
    }
    auto* mpicommhandleobj = (MPI_CommHandleObject*)A.getContentAsHandleScalar();
    if (mpicommhandleobj != nullptr) {
        auto* obj = static_cast<MPI_CommObject*>(mpicommhandleobj->getPointer());
        if (obj != nullptr) {
            commReturned = obj->getComm();
        } else {
            raiseError(L"Nelson:mpi:ERROR_MPI_COMM_VALID_HANDLE_EXPECTED",
                ERROR_MPI_COMM_VALID_HANDLE_EXPECTED);
        }
    } else {
        raiseError(L"Nelson:mpi:ERROR_MPI_COMM_VALID_HANDLE_EXPECTED",
            ERROR_MPI_COMM_VALID_HANDLE_EXPECTED);
    }
    return commReturned;
}
//=============================================================================
ArrayOf
MpiCommToHandle(MPI_Comm mpicomm)
{
    return ArrayOf::handleConstructor(new MPI_CommHandleObject(new MPI_CommObject(mpicomm)));
}
//=============================================================================
bool
MPICommHandleDelete(const ArrayOf& A)
{
    auto deleter = [](MPI_CommHandleObject* mpicommhandleobj) {
        if (mpicommhandleobj) {
            auto* obj = static_cast<MPI_CommObject*>(mpicommhandleobj->getPointer());
            delete obj;
            delete mpicommhandleobj;
        }
    };

    return DeleteHandleObjects<MPI_CommHandleObject>(A, NLS_HANDLE_MPI_COMM_CATEGORY_STR, deleter);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
