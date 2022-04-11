//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicObjectSet.hpp"
#include "Error.hpp"
#include "GOProperty.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
graphicObjectSet(GraphicObject* ptr, const std::string& propertyName, ArrayOf& value)
{
    GOProperty* property = ptr->searchProperty(propertyName);
    ArrayOf res;
    if (property != nullptr) {
        res = ArrayOf::emptyConstructor();
        property->set(value);
        if (ptr->getType() == FIGURE_TYPE_STR) {
            auto* figPtr = (GOFigure*)ptr;
            figPtr->repaint();
        }
        if (ptr->getType() == ROOT_TYPE_STR) {
            auto* rootPtr = (GORoot*)ptr;
            rootPtr->repaint();
        }
    } else {
        Error(_W("Valid property expected."));
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
