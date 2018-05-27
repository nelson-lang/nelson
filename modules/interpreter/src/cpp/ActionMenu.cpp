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
#include "ActionMenu.hpp"
#include "CommandQueue.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
doExit(void)
{
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        eval->addCommandToQueue(L"quit;");
    }
}
//=============================================================================
void
doHelp(void)
{
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        eval->addCommandToQueue(L"doc;");
    }
}
//=============================================================================
void
doPause(void)
{
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        eval->addCommandToQueue(L"keyboard;");
    }
}
//=============================================================================
void
doStop(void)
{
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        eval->addCommandToQueue(L"abort;");
    }
}
//=============================================================================
};
//=============================================================================
