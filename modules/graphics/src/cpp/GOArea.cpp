//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOArea.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
addPropertyIfMissing(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name)
{
    if (go->haveProperty(name)) {
        delete property;
        return;
    }
    go->registerProperty(property, name);
}
//=============================================================================
GOArea::GOArea() : GOPatch()
{
    addPropertyIfMissing(this, new GOLazyDecoratorProperty(GODecoratorKind::Baseline),
        GO_BASE_LINE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOScalarProperty, GO_BASE_VALUE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOAutoManualProperty, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOOnOffProperty, GO_SHOW_BASE_LINE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addPropertyIfMissing(this, new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AREA_STR);
    setScalarDoubleDefault(GO_BASE_VALUE_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(GO_FACE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_SHOW_BASE_LINE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    sortProperties();
}
//=============================================================================
GOArea::~GOArea() = default;
//=============================================================================
std::wstring
GOArea::getType()
{
    return GO_PROPERTY_VALUE_AREA_STR;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
