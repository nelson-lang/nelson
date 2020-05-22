//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
                        io->outputMessage(
                            "  " + GOType + " (" + std::to_string(goFigure->id()) + ")\n");
                        io->outputMessage(goFigure->displayProperties());
                    } else if (GOType == ROOT_TYPE_STR) {
                        io->outputMessage("  " + GOType + "\n");
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
