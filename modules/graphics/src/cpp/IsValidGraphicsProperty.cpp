//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsValidGraphicsProperty.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
#include "GOImage.hpp"
#include "GOPatch.hpp"
#include "GOLineSeries.hpp"
#include "GOUIControl.h"
#include "GOText.hpp"
#include "GOGroup.hpp"
#include "GOContour.hpp"
#include "GOSurface.hpp"
#include "StringHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsValidGraphicsProperty(const std::wstring& GOTypename, const std::wstring& GOPropertyName)
{
    GraphicsObject* fp = nullptr;
    if (StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_AXES_STR)) {
        fp = new GOAxis();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_LINE_STR)) {
        fp = new GOLineSeries();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_IMAGE_STR)) {
        fp = new GOImage();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_ROOT_STR)) {
        fp = new GORoot();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_TEXT_STR)) {
        fp = new GOText();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_FIGURE_STR)) {
        fp = new GOFigure(nullptr, -1);
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_PATCH_STR)) {
        fp = new GOPatch();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_HGGROUP_STR)) {
        fp = new GOGroup();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_UICONTROL_STR)) {
        fp = new GOUIControl();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_CONTOUR_STR)) {
        fp = new GOContour();
    }
    if (!fp && StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_SURFACE_STR)) {
        fp = new GOSurface();
    }
    if (!fp) {
        Error(_W("Invalid Graphics object type name:") + GOTypename);
        return false;
    }

    std::vector<std::wstring> propertyNames = fp->getFieldnames();
    bool isValidPropertyName = false;
    for (auto& name : propertyNames) {
        if (StringHelpers::iequals(name, GOPropertyName)) {
            isValidPropertyName = true;
            break;
        }
    }
    if (fp) {
        delete fp;
        fp = nullptr;
    }
    return isValidPropertyName;
}
//=============================================================================
}
//=============================================================================
