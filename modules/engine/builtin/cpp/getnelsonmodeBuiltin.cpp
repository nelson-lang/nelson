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
#include "getnelsonmodeBuiltin.hpp"
#include "Error.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::EngineGateway::getnelsonmodeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    NELSON_ENGINE_MODE _mode = (NELSON_ENGINE_MODE)eval->getNelsonEngineMode();
    switch (_mode) {
    case BASIC_ENGINE: {
        retval.push_back(ArrayOf::characterArrayConstructor("BASIC_ENGINE"));
    } break;
    case ADVANCED_ENGINE: {
        retval.push_back(ArrayOf::characterArrayConstructor("ADVANCED_ENGINE"));
    } break;
    case BASIC_TERMINAL: {
        retval.push_back(ArrayOf::characterArrayConstructor("BASIC_TERMINAL"));
    } break;
    case ADVANCED_TERMINAL: {
        retval.push_back(ArrayOf::characterArrayConstructor("ADVANCED_TERMINAL"));
    } break;
    case GUI: {
        retval.push_back(ArrayOf::characterArrayConstructor("GUI"));
    } break;
    default: {
        Error(_W("unknown mode."));
    } break;
    }
    return retval;
}
//=============================================================================
