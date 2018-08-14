//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "audioplayer_propertiesBuiltin.hpp"
#include "AudioplayerObject.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audioplayer_propertiesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != AUDIOPLAYER_CATEGORY_STR) {
        Error(_W("audioplayer handle expected."));
    }
    AudioplayerObject* objPlayer = (AudioplayerObject*)param1.getContentAsHandleScalar();
    wstringVector fieldnames = objPlayer->fieldnames();
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring msg;
            if (fieldnames.size() == 0) {
                msg = _W("No property for class: audioplayer.") + L"\n";
            } else {
                msg = _W("Properties for class: audioplayer:") + L"\n\n";
                for (size_t k = 0; k < fieldnames.size(); k++) {
                    msg = msg + std::wstring(L"\t") + fieldnames[k] + std::wstring(L"\n");
                }
                msg = msg + std::wstring(L"\n");
            }
            io->outputMessage(msg);
        }
    } else {
        retval.push_back(ToCellStringAsColumn(fieldnames));
    }
    return retval;
}
//=============================================================================
