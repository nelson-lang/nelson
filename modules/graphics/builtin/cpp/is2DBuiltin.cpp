//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "is2DBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
is2DBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(_W("Invalid object."));
    }
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* gop = findGraphicsObject(handle);
    if (!(gop->getType() == GO_PROPERTY_VALUE_AXES_STR)) {
        Error(_("axes graphics object expected."));
    }
    GOAxis* axis = (GOAxis*)gop;
    GOGObjectsProperty* cp = (GOGObjectsProperty*)axis->findProperty(GO_PARENT_PROPERTY_NAME_STR);
    if (cp) {
        std::vector<int64> parents = cp->data();
        for (auto parent : parents) {
            if (parent > HANDLE_ROOT_OBJECT && parent < HANDLE_OFFSET_OBJECT) {
                GOFigure* fig = findGOFigure(parent);
                if (fig) {
                    fig->repaint();
                }
            }
        }
    }
    retval << ArrayOf::logicalConstructor(axis->is2D());
    return retval;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
