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
        Error(_W("MPI_Comm handle expected."));
    }
    auto* mpicommhandleobj = (MPI_CommHandleObject*)A.getContentAsHandleScalar();
    if (mpicommhandleobj != nullptr) {
        auto* obj = static_cast<MPI_CommObject*>(mpicommhandleobj->getPointer());
        if (obj != nullptr) {
            commReturned = obj->getComm();
        } else {
            Error(_W("MPI_Comm valid handle expected."));
        }
    } else {
        Error(_W("MPI_Comm valid handle expected."));
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
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            auto* qp = (nelson_handle*)A.getDataPointer();
            indexType elementCount = dims.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != NLS_HANDLE_MPI_COMM_CATEGORY_STR) {
                        Error(_W("MPI_Comm handle expected."));
                    }
                    auto* mpicommhandleobj = (MPI_CommHandleObject*)hlObj;
                    auto* obj = static_cast<MPI_CommObject*>(mpicommhandleobj->getPointer());
                    delete obj;
                    delete mpicommhandleobj;
                    HandleManager::getInstance()->removeHandle(hl);
                    res = true;
                }
            }
        } else {
            Error(_W("MPI_Comm valid handle expected."));
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
