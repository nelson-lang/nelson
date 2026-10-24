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
#include "findfigsBuiltin.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
isOffscreen(GOFigure* fig)
{
    std::vector<double> pos = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    std::vector<double> screen
        = getGraphicsRootObject()->findVectorDoubleProperty(GO_SCREEN_SIZE_PROPERTY_NAME_STR);
    if (pos.size() < 4 || screen.size() < 4) {
        return false;
    }
    double x = pos[0];
    double y = pos[1];
    double w = pos[2];
    double h = pos[3];
    return (x + w < screen[0]) || (y + h < screen[1]) || (x > screen[0] + screen[2])
        || (y > screen[1] + screen[3]);
}
//=============================================================================
ArrayOfVector
findfigsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);

    std::vector<int64> handles;
    for (int64 handle : getFigureGraphicsObjects()) {
        GOFigure* fig = findGOFigure(handle);
        if (!fig) {
            continue;
        }
        if (fig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
            && isOffscreen(fig)) {
            handles.push_back(handle);
        }
    }

    nelson_handle* ptr = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, handles.size(), stringVector(), true));
    for (size_t k = 0; k < handles.size(); ++k) {
        ptr[k] = handles[k];
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_GO_HANDLE, Dimensions(handles.size(), 1), ptr);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
