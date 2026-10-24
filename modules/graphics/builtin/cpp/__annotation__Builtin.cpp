//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "__annotation__Builtin.hpp"
#include "GOAnnotation.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObjectFactory.hpp"
#include "RefreshFigure.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static GOFigure*
figureFromHandle(int64 handle)
{
    GOFigure* fig = findGOFigure(handle);
    if (!fig) {
        Error(L"Nelson:graphics:parentMustBeFigure", L"Parent must be a figure.");
    }
    return fig;
}
//=============================================================================
static void
appendFigureChild(GOFigure* fig, int64 childHandle)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> values = children->data();
    if (std::find(values.begin(), values.end(), childHandle) == values.end()) {
        values.push_back(childHandle);
        children->data(values);
    }
}
//=============================================================================
static void
applyProperties(GOAnnotation* annotation, ArrayOfVector values)
{
    while (values.size() >= 2) {
        std::wstring propname(values[0].asWideString());
        try {
            if (!annotation->isWritable(propname) && annotation->haveProperty(propname)) {
                Error(
                    L"Nelson:annotation:readOnlyProperty", L"Property is read-only: %s", propname);
            }
            GOGenericProperty* prop = annotation->findProperty(propname);
            ArrayOf oldValue = prop->get();
            prop->set(values[1]);
            try {
                annotation->updateState();
            } catch (...) {
                try {
                    prop->set(oldValue);
                } catch (...) {
                }
                // A successful updateState() clears the changed flags; a failed one
                // must too, so the restored property is not left marked modified and
                // misread by the next updateState().
                annotation->clearAllChanged();
                throw;
            }
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:annotation:propertyError");
        }
        values.pop_front();
        values.pop_front();
    }
    if (!values.empty()) {
        Error(L"Nelson:graphics:pvPairsExpected", L"Property/value pairs expected.");
    }
}
//=============================================================================
ArrayOfVector
__annotation__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    std::wstring kindName = argIn[0].asWideString();
    int64 parentHandle = argIn[1].asGraphicsObjectScalar();
    GOFigure* fig = figureFromHandle(parentHandle);

    GOAnnotation* annotation = dynamic_cast<GOAnnotation*>(newGraphicsObjectForType(kindName, fig));
    if (!annotation) {
        Error(L"Nelson:annotation:unsupportedType", L"Unsupported annotation type.");
    }
    int64 annotationHandle = assignGraphicsObject(annotation);
    annotation->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, parentHandle);
    appendFigureChild(fig, annotationHandle);

    ArrayOfVector properties(argIn);
    properties.pop_front();
    properties.pop_front();
    applyProperties(annotation, properties);
    annotation->updateState();

    GOCallbackProperty* createCallback = static_cast<GOCallbackProperty*>(
        annotation->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (createCallback) {
        createCallback->executeNow(annotation);
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        refreshFigure(fig);
    }

    ArrayOfVector retval;
    retval << graphicsObjectHandle(annotationHandle);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
