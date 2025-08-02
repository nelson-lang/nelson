//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PostCommand.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
postCommand(const std::wstring& commandToExecute)
{
    void* veval = NelsonConfiguration::getInstance()->getMainEvaluator();
    if (veval != nullptr) {
        std::wstring _cmd = commandToExecute + L";";
        auto* eval = static_cast<Evaluator*>(veval);
        eval->addCommandToQueue(_cmd, true);
        Interface* io = eval->getInterface();
        if (io != nullptr) {
            io->interruptGetLineByEvent();
        }
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
