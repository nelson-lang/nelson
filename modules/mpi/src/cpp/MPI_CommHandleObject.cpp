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
#include "MPI_CommHandleObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
MPI_CommHandleObject::MPI_CommHandleObject(void* _ptr)
    : HandleGenericObject(std::wstring(MPI_COMM_CATEGORY_STR), _ptr, false)
{}
//=============================================================================
MPI_CommHandleObject::~MPI_CommHandleObject() = default;
//=============================================================================
MPI_Comm
HandleToMpiComm(ArrayOf A)
{
    MPI_Comm commReturned = MPI_COMM_NULL;
    if (A.getHandleCategory() != MPI_COMM_CATEGORY_STR) {
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
MPICommHandleDelete(ArrayOf A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            auto* qp = (nelson_handle*)A.getDataPointer();
            for (size_t k = 0; k < dims.getElementCount(); k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != MPI_COMM_CATEGORY_STR) {
                        Error(_W("MPI_Comm handle expected."));
                    }
                    auto* mpicommhandleobj = (MPI_CommHandleObject*)hlObj;
                    if (mpicommhandleobj != nullptr) {
                        auto* obj = static_cast<MPI_CommObject*>(mpicommhandleobj->getPointer());

                        delete obj;

                    } else {
                        Error(_W("MPI_Comm valid handle expected."));
                    }
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
