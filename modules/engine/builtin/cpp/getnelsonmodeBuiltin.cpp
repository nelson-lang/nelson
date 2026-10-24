//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getnelsonmodeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelSon_engine_mode.h"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::EngineGateway::getnelsonmodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    auto _mode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    switch (_mode) {
    case BASIC_ENGINE: {
        retval << ArrayOf::characterArray("BASIC_ENGINE");
    } break;
    case ADVANCED_ENGINE: {
        retval << ArrayOf::characterArray("ADVANCED_ENGINE");
    } break;
    case BASIC_TERMINAL: {
        retval << ArrayOf::characterArray("BASIC_TERMINAL");
    } break;
    case ADVANCED_TERMINAL: {
        retval << ArrayOf::characterArray("ADVANCED_TERMINAL");
    } break;
    case GUI: {
        retval << ArrayOf::characterArray("GUI");
    } break;
    case WEB_GUI: {
        retval << ArrayOf::characterArray("WEB_GUI");
    } break;
    case WASM_TERMINAL: {
        retval << ArrayOf::characterArray("WASM_TERMINAL");
    } break;
    default: {
        Error(L"Nelson:getnelsonmode:unknownMode", L"unknown mode.");
    } break;
    }
    return retval;
}
//=============================================================================
