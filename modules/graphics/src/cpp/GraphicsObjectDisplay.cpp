//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPropertyNames.hpp"
#include "GraphicsObjectDisplay.hpp"
#include "GraphicsObject.hpp"
#include "StringHelpers.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GORoot.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
graphicsObjectDisplay(Interface* io, const Dimensions& dims, nelson_handle* ptrGO)
{
    if (dims.isScalar()) {
        if (ptrGO != nullptr) {
            int64 handle = (int64)ptrGO[0];
            GraphicsObject* fp = nullptr;
            if (isDeletedGraphicsObject(handle)) {
                io->outputMessage("\n");
                io->outputMessage(L"  " + _W("Deleted graphics_object\n"));
                return;
            }
            if (handle == HANDLE_ROOT_OBJECT) {
                fp = getGraphicsRootObject();
            } else if (handle >= HANDLE_OFFSET_OBJECT) {
                fp = findGraphicsObject(handle);
            } else {
                fp = (GraphicsObject*)findGOFigure(handle);
            }
            if (!fp) {
                Error(_W("Invalid handle."));
            }
            wstringVector names = fp->getFieldnames();
            io->outputMessage(L"\n");
            GOGenericProperty* hpType = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR, false);
            GOGenericProperty* hpNumber = fp->findProperty(GO_NUMBER_PROPERTY_NAME_STR, false);
            if (hpType && hpNumber) {
                io->outputMessage(L"  " + hpType->get().getContentAsWideString() + L" ("
                    + hpNumber->toWideString() + L") " + _W("with properties:") + L"\n");
                io->outputMessage(L"\n");
            } else if (hpType) {
                io->outputMessage(L"  " + hpType->get().getContentAsWideString() + L" "
                    + _W("with properties:") + L"\n");
                io->outputMessage(L"\n");
            }

            for (auto& name : names) {
                GOGenericProperty* hp = fp->findProperty(name, false);
                if (hp) {
                    io->outputMessage(L"  " + name + L": " + hp->toWideString() + L"\n");
                } else {
                    io->outputMessage(L"  " + name + L"\n");
                }
            }
        }
    } else {
        if (dims.isColumnVector()) {
            io->outputMessage("\n");
            for (size_t k = 0; k < dims.getElementCount(); ++k) {
                int64 handle = (int64)ptrGO[k];
                GraphicsObject* fp = nullptr;
                if (isDeletedGraphicsObject(handle)) {
                    io->outputMessage(L"  " + _W("Deleted graphics_object\n"));
                } else {
                    if (handle == HANDLE_ROOT_OBJECT) {
                        fp = getGraphicsRootObject();
                    } else if (handle >= HANDLE_OFFSET_OBJECT) {
                        fp = findGraphicsObject(handle);
                    } else {
                        fp = (GraphicsObject*)findGOFigure(handle);
                    }
                    if (!fp) {
                        Error(_W("Invalid handle."));
                    }
                    GOGenericProperty* hpType = fp->findProperty(GO_TYPE_PROPERTY_NAME_STR, false);
                    if (hpType) {
                        io->outputMessage(L"  " + hpType->get().getContentAsWideString() + L"\n");
                    }
                }
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
