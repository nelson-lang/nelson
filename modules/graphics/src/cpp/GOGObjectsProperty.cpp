//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPropertyNames.hpp"
#include "GOGObjectsProperty.hpp"
#include "GraphicsObject.hpp"
#include "GORoot.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOGObjectsProperty::GOGObjectsProperty() { }
//=============================================================================
std::vector<int64>
GOGObjectsProperty::data()
{
    return _data;
}
//=============================================================================
void
GOGObjectsProperty::data(const std::vector<int64>& m)
{
    _data = m;
}
//=============================================================================
ArrayOf
GOGObjectsProperty::get()
{
    nelson_handle* dp = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_GO_HANDLE, _data.size());
    ArrayOf ret = ArrayOf(NLS_GO_HANDLE, Dimensions(_data.size(), 1), dp);
    for (size_t i = 0; i < _data.size(); i++) {
        dp[i] = _data[i];
    }
    return ret;
}
//=============================================================================
void
GOGObjectsProperty::set(ArrayOf arg)
{
    if (arg.isEmpty()) {
        _data.clear();
        GOGenericProperty::set(arg);
        return;
    }
    if (!arg.isGraphicsObject()) {
        Error(_W("Expecting handle for property."));
    }
    const int64* dp = static_cast<const int64*>(
        const_cast<void*>(static_cast<const void*>(arg.getDataPointer())));

    for (indexType i = 0; i < arg.getElementCount(); i++) {
        validateGO(dp[i]);
    }
    _data.clear();
    _data.reserve(arg.getElementCount());
    for (indexType i = 0; i < arg.getElementCount(); i++) {
        _data.push_back(dp[i]);
    }
    GOGenericProperty::set(arg);
}
//=============================================================================
std::wstring
GOGObjectsProperty::toWideString()
{
    std::wstring typeStr;
    for (size_t k = 0; k < _data.size(); k++) {
        GraphicsObject* fp = nullptr;

        if (_data[k] == HANDLE_ROOT_OBJECT) {
            fp = getGraphicsRootObject();
            fp->updateState();
        } else if (_data[k] >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(_data[k], false);
        } else {
            fp = static_cast<GraphicsObject*>(findGOFigure(_data[k]));
        }
        if (fp) {
            GOGenericProperty* hpType = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR);
            if (k == 0) {
                typeStr = hpType->get().getContentAsWideString();
            } else {
                if (typeStr != hpType->get().getContentAsWideString()) {
                    typeStr = GO_CHILDREN_PROPERTY_NAME_STR;
                    break;
                }
            }
        } else {
            typeStr = _W("Deleted graphics_object");
        }
    }
    std::wstring colsStr;
    if (_data.size() == 0) {
        colsStr = L"x0";
    } else {
        colsStr = L"x1";
    }
    std::wstring text;
    if (typeStr.empty()) {
        text = L"[" + std::to_wstring(_data.size()) + colsStr + L"]";
    } else {
        text = L"[" + std::to_wstring(_data.size()) + colsStr + L" " + typeStr + L"]";
    }
    return text;
}
//=============================================================================
}
//=============================================================================
