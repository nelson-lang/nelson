//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__axis_categorical__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOAxis.hpp"
#include "GOCategoricalVectorProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GraphicsObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static ArrayOf
rowVector(const std::vector<double>& values)
{
    ArrayOf result(ArrayOf::doubleRowVector(values.size()));
    double* data = static_cast<double*>(result.readWriteDataPointer());
    for (size_t k = 0; k < values.size(); ++k) {
        data[k] = values[k];
    }
    return result;
}
//=============================================================================
static ArrayOf
categoricalObject(const std::vector<double>& codes, const wstringVector& labels)
{
    wstringVector fieldNames = { L"codes", L"categoryNames", L"isProtected", L"isOrdinal" };
    ArrayOfVector values;
    values << rowVector(codes);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(labels);
    values << ArrayOf::logicalScalar(false);
    values << ArrayOf::logicalScalar(false);
    return ArrayOf::classArray(L"categorical", fieldNames, values);
}
//=============================================================================
static void
setCategoricalProperty(GraphicsObject* axis, const std::wstring& propertyName, const ArrayOf& value)
{
    GOGenericProperty* property = axis->findProperty(propertyName);
    if (auto* categorical = dynamic_cast<GOCategoricalVectorProperty*>(property)) {
        categorical->categoricalValue(value);
        return;
    }
    if (auto* categorical = dynamic_cast<GOCategoricalTwoVectorProperty*>(property)) {
        categorical->categoricalValue(value);
        return;
    }
    Error(L"Nelson:graphics:categoricalAxisUnsupported", L"Categorical axis property expected.");
}
//=============================================================================
static GOAxis*
axisFromArgument(const ArrayOf& value)
{
    if (!value.isGraphicsObject() || !value.isScalar()) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes graphic object expected.");
    }
    auto* handles = static_cast<const nelson_handle*>(value.dataPointer());
    GraphicsObject* object = graphicsObjectFromHandle(handles[0], true);
    GOAxis* axis = dynamic_cast<GOAxis*>(object);
    if (!axis) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes graphic object expected.");
    }
    return axis;
}
//=============================================================================
ArrayOfVector
__axis_categorical__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3);
    GOAxis* axis = axisFromArgument(argIn[0]);
    std::wstring dimension = argIn[1].asWideString();
    wstringVector labels;
    uniformizeStringVector(argIn[2], labels);
    if (labels.empty()) {
        return {};
    }

    std::vector<double> tickCodes;
    tickCodes.reserve(labels.size());
    for (size_t k = 0; k < labels.size(); ++k) {
        tickCodes.push_back(static_cast<double>(k + 1));
    }
    std::vector<double> limitCodes = { 1.0, static_cast<double>(labels.size()) };

    ArrayOf categoricalTicks = categoricalObject(tickCodes, labels);
    ArrayOf categoricalLimits = categoricalObject(limitCodes, labels);
    if (StringHelpers::iequals(dimension, L"x")) {
        setCategoricalProperty(axis, GO_X_TICK_PROPERTY_NAME_STR, categoricalTicks);
        setCategoricalProperty(axis, GO_X_LIM_PROPERTY_NAME_STR, categoricalLimits);
    } else if (StringHelpers::iequals(dimension, L"y")) {
        setCategoricalProperty(axis, GO_Y_TICK_PROPERTY_NAME_STR, categoricalTicks);
        setCategoricalProperty(axis, GO_Y_LIM_PROPERTY_NAME_STR, categoricalLimits);
    } else {
        Error(L"Nelson:graphics:invalidAxisDimension", L"Axis dimension must be 'x' or 'y'.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
