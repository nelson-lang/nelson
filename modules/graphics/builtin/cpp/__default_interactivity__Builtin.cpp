//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__default_interactivity__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__default_interactivity__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // __default_interactivity__(ax, 'on'|'off')
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);

    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(L"Nelson:graphics:interaction:InvalidInputAxes", L"Input must be an axes handle.");
    }
    int64 handle = argIn[0].asGraphicsObjectScalar();
    GraphicsObject* fp = (handle >= HANDLE_OFFSET_OBJECT) ? findGraphicsObject(handle) : nullptr;
    if (fp == nullptr || fp->getType() != GO_PROPERTY_VALUE_AXES_STR) {
        Error(L"Nelson:graphics:interaction:InvalidInputAxes", L"Input must be an axes handle.");
    }
    GOAxis* axis = (GOAxis*)fp;

    std::wstring option = argIn[1].asWideString();
    GOGenericProperty* property
        = axis->findProperty(GO_DEFAULT_INTERACTIVITY_INTERNAL_PROPERTY_NAME_STR, false);
    if (property != nullptr) {
        property->set(ArrayOf::characterArray(option));
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
