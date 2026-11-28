//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ActionMenu.hpp"
#include "CommandQueue.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
doHelp()
{
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval != nullptr) {
        eval->addCommandToQueue(L"doc;");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
