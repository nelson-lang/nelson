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
#include "audioplayer_displayBuiltin.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        Interface* io = eval->getInterface();
        if (io == nullptr) {
            return retval;
        }
        std::string name;
        if (argIn.size() == 2) {
            name = argIn[1].getContentAsCString();
        }
        if (!name.empty()) {
            io->outputMessage("\n");
            io->outputMessage(name + " =\n\n");
        }
        Dimensions dimsParam1 = param1.getDimensions();
        io->outputMessage(L"[audioplayer] - size: ");
        dimsParam1.printMe(io);
        io->outputMessage("\n");
        if (param1.isScalar()) {
            if (param1.getHandleCategory() != AUDIOPLAYER_CATEGORY_STR) {
                Error(_W("audioplayer handle expected."));
            }
            auto* objPlayer = (AudioplayerObject*)param1.getContentAsHandleScalar();
            objPlayer->disp(eval);
        }
        if (!name.empty()) {
            io->outputMessage("\n");
        }
    } else {
        Error(_W("audioplayer handle expected."));
    }
    return retval;
}
//=============================================================================