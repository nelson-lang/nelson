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
#include "GOLimitsInRange.hpp"
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
std::vector<double>
GOArea::getLimitsInRange(const std::vector<double>& box, const std::array<bool, 3>& logScale)
{
    // Area vertices (data and baseline) count when inside the range, without clipping.
    std::vector<double> full(getLimits());
    if (full.empty()) {
        return full;
    }
    ArrayOf vertices(findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR));
    vertices.promoteType(NLS_DOUBLE);
    indexType n = vertices.rows();
    if (n == 0 || vertices.dataPointer() == nullptr) {
        return {};
    }
    indexType nDims = vertices.columns();
    const double* p = (const double*)vertices.dataPointer();
    std::vector<double> xs(p, p + n);
    std::vector<double> ys = (nDims > 1) ? std::vector<double>(p + n, p + 2 * n)
                                         : std::vector<double>(n, 0.);
    std::vector<double> zs;
    if (nDims > 2) {
        zs.assign(p + 2 * n, p + 3 * n);
    }
    LimitsRange range(box, logScale);
    LimitsInRangeAccumulator visible(range);
    visible.addPoints(xs, ys, zs);
    return visible.result(full);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
