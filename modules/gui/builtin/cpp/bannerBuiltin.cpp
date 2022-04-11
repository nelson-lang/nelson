//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bannerBuiltin.hpp"
#include "Banner.hpp"
#include "Error.hpp"
#include "GuiTerminal.hpp"
#include "Interface.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::bannerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 0);
    if (eval) {
        Interface* io = eval->getInterface();
        if (io) {
            auto _mode = static_cast<NELSON_ENGINE_MODE>(eval->getNelsonEngineMode());
            switch (_mode) {
            case GUI: {
                auto* gtio = (GuiTerminal*)io;
                gtio->banner();
            } break;
            default: {
                Banner();
            } break;
            }
        }
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
