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
#include "MacroFunctionDef.hpp"
#include "AstManager.hpp"
#include "Context.hpp"
#include "FileParser.hpp"
#include "ParserInterface.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
#ifdef WIN32
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
MacroFunctionDef::MacroFunctionDef()
{
    localFunction = false;
    nextFunction = nullptr;
    prevFunction = nullptr;
    code = nullptr;
    ptAst.clear();
}
//=============================================================================
MacroFunctionDef::~MacroFunctionDef()
{
    if (nextFunction != nullptr) {
        delete nextFunction;
        nextFunction = nullptr;
    }
    for (auto p : ptAst) {
        if (p != nullptr) {
            deleteAst(p, ptAst);
            p = nullptr;
        }
    }
    ptAst.clear();
    code = nullptr;
    localFunction = false;
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
void
MacroFunctionDef::printMe(Interface* io)
{
    stringVector tmp;
    snprintf(msgBuffer, MSGBUFLEN, _("Function name:%s\n").c_str(), name.c_str());
    io->outputMessage(msgBuffer);
    io->outputMessage(_W("Function class: Compiled M function\n"));
    io->outputMessage(_W("returnVals: "));
    tmp = returnVals;
    size_t i;
    for (i = 0; i < tmp.size(); i++) {
        snprintf(msgBuffer, MSGBUFLEN, "%s ", tmp[i].c_str());
        io->outputMessage(msgBuffer);
    }
    io->outputMessage("\n");
    io->outputMessage(_W("arguments: "));
    tmp = arguments;
    for (i = 0; i < tmp.size(); i++) {
        snprintf(msgBuffer, MSGBUFLEN, "%s ", tmp[i].c_str());
        io->outputMessage(msgBuffer);
    }
    io->outputMessage("\ncode: \n");
    printAST(code);
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateFunction(Evaluator* eval, ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    ArrayOf a;
    size_t minCount = 0;
    Context* context = eval->getContext();
    std::string filenameUtf8 = wstring_to_utf8(fileName);
    context->pushScope(name);
    eval->pushDebug(filenameUtf8, name);
    // Push our local functions onto the function scope
    MacroFunctionDef* cp;
    // Walk up until we get to the head of the list
    cp = this;
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
        minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            if (arg[0] == '&') {
                arg.erase(0, 1);
            }
            context->insertVariableLocally(arg, inputs[i]);
        }
        // context->insertVariableLocally("nargin",
        // ArrayOf::doubleConstructor((double)minCount));
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        // Count the number of supplied arguments
        size_t inputCount = inputs.size();
        // context->insertVariableLocally("nargin",
        // ArrayOf::doubleConstructor((double)inputCount));
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
            if (arg[0] == '&') {
                arg.erase(0, 1);
            }
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
    // context->insertVariableLocally("nargout",
    // ArrayOf::doubleConstructor(nargout));
    context->getCurrentScope()->setNargOut(nargout);
    uint64 tic = 0;
    try {
        uint64 tic = Profiler::getInstance()->tic();
        eval->block(code);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->fileName, false);
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
            outputs = ArrayOfVector(returnVals.size());
            for (size_t i = 0; i < returnVals.size(); i++) {
                if (!context->lookupVariableLocally(returnVals[i], a)) {
                    if (!warningIssued) {
                        std::wstring message = _W("Function") + L" : " + utf8_to_wstring(name)
                            + L"\n" + WARNING_OUTPUTS_NOT_ASSIGNED;
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
                    Error(_W("The special variable 'varargout' was not defined as a "
                             "cell-array."));
                }
            }
            indexType varlen = varargout.getLength();
            int explicitCount = static_cast<int>(returnVals.size()) - 1;
            bool noArgs = (explicitCount == 0 && varlen == 0);
            if (!noArgs && !haveVarargout) {
                Error(_W("The special variable 'varargout' was not defined as expected."));
            }
            if (explicitCount == 0 && varlen > 0 && nargout < 2) {
                indexType toFill = 1;
                outputs = ArrayOfVector(toFill);
                const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
                // Get the length
                if (static_cast<indexType>(toFill)
                    > static_cast<indexType>(varargout.getElementCount())) {
                    Error(_W("Not enough outputs in varargout to satisfy call."));
                }
                outputs[0] = dp[0];
            } else {
                outputs = ArrayOfVector(nargout);
                // For each explicit argument (that we have), insert it
                // into the scope.
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
        // Check for arguments that were passed by reference, and
        // update their values.
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            if (arg[0] == '&') {
                arg.erase(0, 1);
            }
            context->lookupVariableLocally(arg, inputs[i]);
        }
        context->popScope();
        eval->popDebug();
        return outputs;
    } catch (const Exception&) {
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->fileName, false);
            Profiler::getInstance()->toc(tic, stack);
        }
        context->popScope();
        eval->popDebug();
        throw;
    }
}
//=============================================================================
std::string
MacroFunctionDef::getCompleteName()
{
    if (this->localFunction) {
        MacroFunctionDef* pF = this->prevFunction;
        while (pF != nullptr) {
            if (!pF->localFunction) {
                return pF->name + ">" + this->name;
            }
            pF = pF->prevFunction;
        }
    }
    return this->name;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
