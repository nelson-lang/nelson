//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "allchildBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static ArrayOf
handlesToArray(const std::vector<int64>& handles)
{
    return graphicsObjectVector(handles);
}
//=============================================================================
ArrayOfVector
allchildBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }

    std::vector<int64> result;
    const nelson_handle* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    for (indexType k = 0; k < argIn[0].elementCount(); ++k) {
        GraphicsObject* fp = graphicsObjectFromHandle(handles[k], true);
        // Same order as the Children property, hidden handles included.
        std::vector<int64> childHandles = reportedChildren(fp);
        result.insert(result.end(), childHandles.begin(), childHandles.end());
    }

    ArrayOfVector retval;
    retval << handlesToArray(result);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
