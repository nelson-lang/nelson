//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "ExecuteCommand.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
executeCommand(const std::wstring& commandToExecute, bool forceEvaluateString)
{
    void* veval = GetNelsonMainEvaluatorDynamicFunction();
    if (veval != nullptr) {
        std::wstring _cmd = commandToExecute + L";";
        auto* eval = static_cast<Evaluator*>(veval);
        Interface* io = eval->getInterface();
        if (io != nullptr) {
            if (forceEvaluateString) {
                eval->evaluateString(_cmd + L"\n", io->isAtPrompt());
            } else {
                if (io->isAtPrompt()) {
                    eval->addCommandToQueue(_cmd, true);
                } else {
                    eval->evaluateString(_cmd + L"\n");
                }
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
