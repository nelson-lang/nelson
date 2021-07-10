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
#include <boost/format.hpp>
#include "MacroFunctionDef.hpp"
#include "Context.hpp"
#include "FileParser.hpp"
#include "ParserInterface.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
MacroFunctionDef::MacroFunctionDef()
{
    localFunction = false;
    nextFunction = nullptr;
    prevFunction = nullptr;
    code = nullptr;
    ptrAstCodeAsVector.clear();
    isScript = false;
}
//=============================================================================
MacroFunctionDef::~MacroFunctionDef()
{
    if (nextFunction != nullptr) {
        delete nextFunction;
        nextFunction = nullptr;
    }
    AbstractSyntaxTree::deleteReferences(ptrAstCodeAsVector);
    code = nullptr;
    localFunction = false;
    isScript = false;
}
//=============================================================================
int
MacroFunctionDef::nargin()
{
    if (arguments.empty()) {
        return 0;
    }
    if (arguments[arguments.size() - 1] == "varargin") {
        return -static_cast<int>(arguments.size());
    }
    return static_cast<int>(arguments.size());
}
//=============================================================================
int
MacroFunctionDef::inputArgCount()
{
    if (arguments.empty()) {
        return 0;
    }
    if (arguments[arguments.size() - 1] == "varargin") {
        return -1;
    }
    return static_cast<int>(arguments.size());
}
//=============================================================================
int
MacroFunctionDef::nargout()
{
    if (returnVals.empty()) {
        return 0;
    }
    if (returnVals[returnVals.size() - 1] == "varargout") {
        return -static_cast<int>(returnVals.size());
    }
    return static_cast<int>(returnVals.size());
}
//=============================================================================
int
MacroFunctionDef::outputArgCount()
{
    if (returnVals.empty()) {
        return 0;
    }
    if (returnVals[returnVals.size() - 1] == "varargout") {
        return -1;
    }
    return static_cast<int>(returnVals.size());
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    size_t minCount = 0;
    Context* context = eval->getContext();
    context->pushScope(this->getName());

    std::string filenameUtf8 = wstring_to_utf8(this->getFilename());
    eval->callstack.pushDebug(filenameUtf8, this->getName());
    // Push our local functions onto the function scope
    MacroFunctionDef* cp = this;
    while (cp->prevFunction != nullptr) {
        cp = cp->prevFunction;
    }
    cp = cp->nextFunction;
    while (cp != nullptr) {
        context->insertMacroFunctionLocally((FuncPtr)cp);
        cp = cp->nextFunction;
    }
    // When the function is called, the number of inputs is
    // at sometimes less than the number of arguments requested.
    // Check the argument count.  If this is a non-varargin
    // argument function, then use the following logic:
    minCount = 0;
    if (inputArgCount() != -1) {
        if (inputs.size() > arguments.size()) {
            context->popScope();
            eval->callstack.popDebug();
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        // Count the number of supplied arguments
        size_t inputCount = inputs.size();
        size_t nbArgumentsWithoutVarArgIn = arguments.size();
        if (arguments[arguments.size() - 1] == "varargin") {
            nbArgumentsWithoutVarArgIn = nbArgumentsWithoutVarArgIn - 1;
        }
        if (inputCount < nbArgumentsWithoutVarArgIn) {
            context->popScope();
            eval->callstack.popDebug();
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(inputCount));
        // Get the number of explicit arguments
        int explicitCount = static_cast<int>(arguments.size()) - 1;
        // For each explicit argument (that we have an input for),
        // insert it into the scope.
        minCount = (explicitCount < static_cast<int>(inputCount))
            ? static_cast<size_t>(explicitCount)
            : inputCount;
        size_t i;
        for (i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        inputCount -= minCount;
        // Put minCount...inputCount
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(inputCount);
        auto* dp = static_cast<ArrayOf*>(varg.getReadWriteDataPointer());
        for (i = 0; i < inputCount; i++) {
            dp[i] = inputs[i + minCount];
        }
        context->insertVariableLocally("varargin", varg);
    }
    context->getCurrentScope()->setNargOut(nargout);
    uint64 tic = 0;
    try {
        uint64 tic = Profiler::getInstance()->tic();
        eval->block(code);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::emptyConstructor());
        }
        bool warningIssued = false;
        if (outputArgCount() != -1) {
            outputs.resize(returnVals.size());
            ArrayOf a;
            for (size_t i = 0; i < returnVals.size(); i++) {
                if (!context->lookupVariableLocally(returnVals[i], a)) {
                    if (!warningIssued) {
                        std::wstring message = str(boost::wformat(_W("Function : '%s'."))
                            % utf8_to_wstring(this->getName()));
                        message = message + L"\n" + WARNING_OUTPUTS_NOT_ASSIGNED;
                        Warning(message);
                        warningIssued = true;
                    }
                    a = ArrayOf::emptyConstructor();
                }
                outputs[i] = a;
            }
        } else {
            ArrayOf varargout;
            bool haveVarargout = context->lookupVariableLocally("varargout", varargout);
            if (haveVarargout) {
                if (varargout.getDataClass() != NLS_CELL_ARRAY) {
                    context->popScope();
                    eval->callstack.popDebug();
                    Error(_W("The special variable 'varargout' was not defined as a "
                             "cell-array."));
                }
            }
            indexType varlen = varargout.getElementCount();
            int explicitCount = static_cast<int>(returnVals.size()) - 1;
            bool noArgs = (explicitCount == 0 && varlen == 0);
            if (!noArgs && !haveVarargout) {
                context->popScope();
                eval->callstack.popDebug();
                Error(_W("The special variable 'varargout' was not defined as expected."));
            }
            if (explicitCount == 0 && varlen > 0 && nargout < 2) {
                indexType toFill = 1;
                outputs.resize(toFill);
                const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
                // Get the length
                if (static_cast<indexType>(toFill)
                    > static_cast<indexType>(varargout.getElementCount())) {
                    Error(_W("Not enough outputs in varargout to satisfy call."));
                }
                outputs[0] = dp[0];
            } else {
                outputs.resize(nargout);
                // For each explicit argument (that we have), insert it
                // into the scope.
                ArrayOf a;
                for (int i = 0; i < explicitCount; i++) {
                    if (!context->lookupVariableLocally(returnVals[i], a)) {
                        if (!warningIssued) {
                            Warning(_W("one or more outputs not assigned in call."));
                            warningIssued = true;
                        }
                        a = ArrayOf::emptyConstructor();
                    }
                    outputs[i] = a;
                }
                // Are there any outputs not yet filled?
                if (nargout > explicitCount) {
                    // Get the data pointer
                    const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
                    // Get the length
                    int toFill = nargout - explicitCount;
                    if (static_cast<double>(toFill) > static_cast<double>(varlen)) {
                        Error(_W("Not enough outputs in varargout to satisfy call."));
                    }
                    for (int i = 0; i < toFill; i++) {
                        outputs[explicitCount + i] = dp[i];
                    }
                }
            }
        }
        context->popScope();
        eval->callstack.popDebug();
    } catch (const Exception&) {
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        context->popScope();
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMScript(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    Context* context = eval->getContext();
    std::string filenameUtf8 = wstring_to_utf8(this->getFilename());
    eval->callstack.pushDebug(filenameUtf8, this->getName());

    context->getCurrentScope()->setNargIn(0);
    context->getCurrentScope()->setNargOut(0);

    uint64 tic = 0;
    try {
        uint64 tic = Profiler::getInstance()->tic();
        eval->block(code);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::emptyConstructor());
        }
        eval->callstack.popDebug();
    } catch (const Exception&) {
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    if (isScript) {
        return evaluateMScript(eval, inputs, nargout);
    }
    return evaluateMFunction(eval, inputs, nargout);
}
//=============================================================================
std::string
MacroFunctionDef::getCompleteName()
{
    if (this->localFunction) {
        MacroFunctionDef* pF = this->prevFunction;
        while (pF != nullptr) {
            if (!pF->localFunction) {
                return pF->getName() + ">" + this->getName();
            }
            pF = pF->prevFunction;
        }
    }
    return this->getName();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
