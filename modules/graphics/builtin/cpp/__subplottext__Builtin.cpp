//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__subplottext__Builtin.hpp"
#include <algorithm>
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "RefreshFigure.hpp"
#include "GOSubplotText.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
static int64
parentHandle(ArrayOfVector& values)
{
    int64 handle = -1;
    ArrayOfVector kept;
    for (size_t k = 0; k + 1 < values.size(); k += 2) {
        std::wstring propname(values[k].asWideString());
        if (propname == GO_PARENT_PROPERTY_NAME_STR) {
            handle = values[k + 1].asGraphicsObjectScalar();
        } else {
            kept << values[k];
            kept << values[k + 1];
        }
    }
    values = kept;
    if (handle != -1) {
        return handle;
    }
    handle = getCurrentFigure();
    if (handle == -1) {
        handle = createNewFigure();
    }
    return handle;
}
//=============================================================================
static void
applyProperties(GOSubplotText* text, ArrayOfVector values)
{
    while (values.size() >= 2) {
        std::wstring propname(values[0].asWideString());
        try {
            if (propname != GO_AUTO_PARENT_PROPERTY_NAME_STR) {
                if (!text->isWritable(propname) && text->haveProperty(propname)) {
                    Error(L"Nelson:subplottext:readOnlyProperty", L"Property is read-only: %s",
                        propname);
                }
                text->findProperty(propname)->set(values[1]);
                text->updateState();
            }
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:subplottext:propertyError");
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
__subplottext__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    ArrayOfVector properties(argIn);
    int64 parent = parentHandle(properties);
    GOFigure* fig = figureFromHandle(parent);

    GOSubplotText* text = new GOSubplotText;
    int64 textHandle = assignGraphicsObject(text);
    text->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, parent);
    appendFigureChild(fig, textHandle);
    applyProperties(text, properties);
    text->updateState();

    GOCallbackProperty* createCallback
        = static_cast<GOCallbackProperty*>(text->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (createCallback) {
        createCallback->executeNow(text);
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        refreshFigure(fig);
    }

    ArrayOfVector retval;
    retval << graphicsObjectHandle(textHandle);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
