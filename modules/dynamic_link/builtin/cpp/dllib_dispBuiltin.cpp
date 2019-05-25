//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "dllib_dispBuiltin.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dllib_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        if (io) {
            Dimensions dimsParam1 = param1.getDimensions();
            io->outputMessage(L"[dllib] - size: ");
            dimsParam1.printMe(io);
            io->outputMessage("\n");
        }
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != DLLIB_CATEGORY_STR) {
                Error(_W("dllib handle expected."));
            }
            auto* dllibObj = (DynamicLinkLibraryObject*)param1.getContentAsHandleScalar();
            dllibObj->disp(eval);
        }
    } else {
        Error(_W("dllib handle expected."));
    }
    return retval;
}
//=============================================================================
