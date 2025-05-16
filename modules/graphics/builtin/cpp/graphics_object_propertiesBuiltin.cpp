//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_propertiesBuiltin.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_propertiesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
        fp->updateState();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
    } else {
        fp = (GraphicsObject*)findGOFigure(handle);
    }
    if (!fp) {
        Error(_W("Invalid handle."));
    }
    std::vector<std::wstring> propertyNames = fp->getVisibleFieldnames();
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring msg;
            if (propertyNames.size() == 0) {
                msg = L"\n" + _W("No property for class") + L" " + fp->getType() + L":\n";
            } else {
                msg = L"\n" + _W("Properties for class") + L" " + fp->getType() + L":\n\n";
                for (auto& propertyName : propertyNames) {
                    msg = msg + std::wstring(L"\t") + propertyName + std::wstring(L"\n");
                }
                msg = msg + std::wstring(L"\n");
            }
            io->outputMessage(msg);
        }
    } else {
        retval.push_back(ArrayOf::toCellArrayOfCharacterColumnVectors(propertyNames));
    }
    return retval;
}
//=============================================================================
