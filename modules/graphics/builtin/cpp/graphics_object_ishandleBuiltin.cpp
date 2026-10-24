//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_ishandleBuiltin.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
graphics_object_ishandleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    Dimensions dims = argIn[0].dimensions();
    logical* ptrResult = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.elementCount(), stringVector(), false));
    const nelson_handle* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        ptrResult[k] = !isDeletedGraphicsObject(handles[k]);
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_LOGICAL, dims, ptrResult);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
