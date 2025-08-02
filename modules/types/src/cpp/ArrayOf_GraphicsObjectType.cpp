//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isGraphicsObject() const
{
    if (dp) {
        return (dp->dataClass == NLS_GO_HANDLE);
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::graphicsObjectConstructor(go_handle graphicsObjectHandle)
{
    nelson_handle* ptr = static_cast<nelson_handle*>(ArrayOf::allocateArrayOf(NLS_GO_HANDLE, 1));
    Dimensions dims(1, 1);
    ptr[0] = (nelson_handle)graphicsObjectHandle;
    ArrayOf res = ArrayOf(NLS_GO_HANDLE, dims, (void*)ptr);
    return res;
}
//=============================================================================
go_handle
ArrayOf::getContentAsGraphicsObjectScalar() const
{
    if (!isGraphicsObject()) {
        Error(_W("Expected an graphics object."));
    }
    if (!isScalar()) {
        Error(_W("Expected an graphics object scalar."));
    }
    nelson_handle* qp = (nelson_handle*)getDataPointer();
    if (qp == nullptr) {
        Error(_W("Expected a valid handle."));
    }
    return (go_handle)(qp[0]);
}
//=============================================================================
}
//=============================================================================
