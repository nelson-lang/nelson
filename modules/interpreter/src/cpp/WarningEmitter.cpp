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
#include "WarningEmitter.h"
#include "Exception.hpp"
#include "Interface.hpp"
#include "Error.hpp"
#include "DebugStack.hpp"
//=============================================================================
static Nelson::Evaluator* evaluatorWarning = nullptr;
//=============================================================================
namespace Nelson {
//=============================================================================
void
setWarningEvaluator(Evaluator* eval)
{
    evaluatorWarning = eval;
}
//=============================================================================
}
//=============================================================================
void
NelsonWarningEmitter(const wchar_t* msg, const wchar_t* id, bool asError)
{
    std::wstring message = std::wstring(msg);
    std::wstring identifier = std::wstring(id);
    if (!message.empty()) {
        if (asError) {
            Nelson::Error(message, identifier);
        } else {
            if (evaluatorWarning) {
                Nelson::stackTrace trace;
                DebugStack(evaluatorWarning->cstack, 1, trace);
                Nelson::Exception exception(message, trace, identifier);
                evaluatorWarning->setLastWarningException(exception);
                Nelson::Interface* io = evaluatorWarning->getInterface();
                if (io) {
                    io->warningMessage(exception.getMessage());
                }
            }
        }
    }
}
//=============================================================================
