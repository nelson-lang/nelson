//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicObjectDisplay.hpp"
#include "GOProperty.hpp"
#include "ClassName.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
graphicObjectDisplay(Interface* io, const Dimensions& dims, nelson_handle* ptrGO)
{
    if (dims.isScalar() || dims.isColumnVector()) {
        if (ptrGO != nullptr) {
            for (size_t k = 0; k < dims.getElementCount(); ++k) {
                auto* go = (GraphicObject*)NELSON_HANDLE_TO_PTR(ptrGO[k]);
                if ((go == nullptr) || (go->referenceCount() == 0)) {
                    io->outputMessage(L"  " + _W("Deleted graphic_object.\n"));
                } else {
                    std::string GOType = go->getType();
                    if (GOType == FIGURE_TYPE_STR) {
                        auto* goFigure = (GOFigure*)go;
                        io->outputMessage("  " + GOType + " (" + std::to_string(goFigure->id())
                            + ") " + _("with properties:") + "\n");
                        io->outputMessage("\n");
                        io->outputMessage(goFigure->displayProperties());
                    } else if (GOType == ROOT_TYPE_STR) {
                        io->outputMessage("  " + GOType + " " + _("with properties:") + "\n");
                        io->outputMessage("\n");
                        auto* goRoot = (GORoot*)go;
                        io->outputMessage(goRoot->displayProperties());
                    } else {
                        io->outputMessage("  " + GOType + "\n");
                    }
                }
            }
        }
    } else {
        io->outputMessage("  " + dims.toString() + " " + NLS_GO_HANDLE_STR + "\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
