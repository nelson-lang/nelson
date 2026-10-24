//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <atomic>
#include <mutex>
#include <regex>
#include <unordered_map>
#include <unordered_set>
#include "BytecodeCompiler.hpp"
#include "BytecodeVM.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "MacroFunctionDef.hpp"
#include "MacroFunctionArgumentsLowering.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "FileParser.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "ContainersMapFastPaths.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadDispatchArgument.hpp"
#include "StringHelpers.hpp"
#include "OnCleanupObjectHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    bytecodeNeedsContextScope(Evaluator* eval, const BytecodeChunk& chunk)
    {
        if (chunk.isScript) {
            return true;
        }
        for (const Instruction& ins : chunk.code) {
            switch (ins.op) {
            case OpCode::LOAD_GLOBAL:
            case OpCode::LOAD_GLOBAL_OR_JUMP:
            case OpCode::STORE_GLOBAL:
            case OpCode::LOAD_ANS:
            case OpCode::STORE_ANS:
            case OpCode::DELETE_LOCAL:
            case OpCode::CALL_BUILTIN:
            case OpCode::CALL_MACRO:
            case OpCode::CALL_HANDLE:
            case OpCode::MAKE_FH_NAMED:
            case OpCode::MAKE_FH_ANONYMOUS:
            case OpCode::DECLARE_IMPORT:
            case OpCode::TRY_BEGIN:
            case OpCode::CATCH_BEGIN:
                return true;
            case OpCode::CALL_NAMED: {
                if (ins.A >= chunk.names.size()) {
                    return true;
                }
                const std::string& calleeName = chunk.names[ins.A];
                if (calleeName == chunk.functionName && chunk.selfFunction != nullptr
                    && !chunk.selfFunction->isOverload()) {
                    break;
                }
                // Display calls write ans into the current scope (VM PRINT
                // path); the scope must exist and be this function's.
                if ((ins.flags & INST_FLAG_PRINT) != 0) {
                    return true;
                }
                // A call through a name that is also a local slot can resolve
                // to a runtime variable (function handle): not analyzable.
                if (std::find(chunk.localNames.begin(), chunk.localNames.end(), calleeName)
                    != chunk.localNames.end()) {
                    return true;
                }
                FunctionDef* calleeDef = nullptr;
                if (eval == nullptr || !eval->lookupFunction(calleeName, calleeDef)
                    || calleeDef == nullptr) {
                    return true;
                }
                // A callee that never needs the caller context (transitively)
                // cannot observe this function's scope, so its absence is not
                // detectable; the VM still pushes one lazily if a runtime
                // dispatch (e.g. overload) picks a different, needier callee.
                if (BytecodeVM::functionNeedsCallerContext(eval, calleeDef, false)
                    || BytecodeVM::functionNeedsCallerContext(eval, calleeDef, true)) {
                    return true;
                }
                break;
            }
            default:
                break;
            }
        }
        return false;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    struct RecursionGuard
    {
        int& depth;
        explicit RecursionGuard(int& d) : depth(d) { ++depth; }
        ~RecursionGuard() { --depth; }
        RecursionGuard(const RecursionGuard&) = delete;
        RecursionGuard&
        operator=(const RecursionGuard&)
            = delete;
    };
    static thread_local int recursionDepth = 0;
    RecursionGuard rg(recursionDepth);

    // A local sub/nested function is private to its file and is never a dispatch
    // target: it runs its own body even when called with a class argument that
    // has a same-named method.
    if (eval->withOverload && inputs.size() > 0 && !this->isOverload()
        && this->overloadAutoMode == NLS_OVERLOAD_AUTO_ON && !this->localFunction
        && !this->nestedFunction) {
        // The overload probe result is cached per basic dispatch-argument type:
        // for these types the overload name only depends on the data class, so
        // a miss stays a miss until the function tables change.
        const ArrayOf& dispatchInput = inputs[overloadDispatchArgumentIndex(inputs, getName())];
        const NelsonType dispatchType = dispatchInput.dataClass();
        const int probeCacheBit = (!dispatchInput.isSparse() && dispatchType < NLS_CLASS_ARRAY)
            ? (int)dispatchType
            : -1;
        const uint64 functionTablesGeneration = FunctionsInMemory::getInstance()->generation();
        bool skipProbe = false;
        if (probeCacheBit >= 0 && overloadProbeGeneration == functionTablesGeneration
            && ((overloadProbeMissMask >> probeCacheBit) & 1u) != 0) {
            skipProbe = true;
        }
        if (!skipProbe) {
            // isKey(M, key) on a char-key containers.Map in native storage
            if (dispatchType == NLS_HANDLE && inputs.size() == 2 && nargout <= 1
                && getName() == "isKey") {
                ArrayOfVector keyResult;
                if (tryContainersMapIsKey(dispatchInput, inputs[1], keyResult)) {
                    return keyResult;
                }
            }
            bool wasFound = false;
            ArrayOfVector res = callOverloadedFunction(eval,
                NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nargout,
                inputs, getName(), ClassName(dispatchInput), dispatchType, wasFound);
            if (wasFound) {
                return res;
            }
            if (probeCacheBit >= 0) {
                if (overloadProbeGeneration != functionTablesGeneration) {
                    overloadProbeGeneration = functionTablesGeneration;
                    overloadProbeMissMask = 0;
                }
                overloadProbeMissMask |= (1u << probeCacheBit);
            }
        }
        // callOverloadedFunction above handles the exact-class overload (including
        // an external @Class/method.m). Only an overload inherited from a
        // superclass is missed (e.g. chebtech2 inherits diff from chebtech).
        // Limiting this resolution to classes with superclasses keeps the common
        // case off the path-touching resolver, which does not cache misses and
        // would stat the filesystem on every call.
        if (dispatchInput.isClassType() || dispatchInput.isHandle()) {
            const std::string currentType = dispatchInput.isHandle()
                ? dispatchInput.handleCategory()
                : dispatchInput.classType();
            if (!currentType.empty() && !classdefSuperclasses(currentType).empty()) {
                std::string inheritedOverloadName;
                if (classdefResolveMethodFunction(currentType, getName(), inheritedOverloadName,
                        eval->getClassdefAccessContext())) {
                    FunctionDef* inheritedDef = nullptr;
                    if (eval->getContext()->lookupFunction(inheritedOverloadName, inheritedDef)
                        && inheritedDef != nullptr && inheritedDef != this) {
                        return inheritedDef->evaluateFunction(eval, inputs, nargout);
                    }
                }
            }
        }
    }

    ArrayOfVector outputs;
    ensureBytecodeCompiled();
    if (canEvaluateScopeFree(eval)) {
        const std::string& filenameUtf8 = this->getFilenameUtf8();
        eval->callstack.pushDebug(filenameUtf8, getCompleteName());
        uint64 tic = 0;
        const bool profileThisCall = (recursionDepth == 1) || this->localFunction;
        try {
            if (profileThisCall) {
                tic = Profiler::getInstance()->tic();
            }
            outputs = BytecodeVM::executeFunction(eval, *bytecodeChunk, inputs, nargout, false);
            if (profileThisCall && tic != 0) {
                internalProfileFunction stack
                    = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
                Profiler::getInstance()->toc(tic, stack);
            }
            State state(eval->getState());
            if (state < NLS_STATE_QUIT) {
                eval->resetState();
            }
            eval->callstack.popDebug();
            if (state == NLS_STATE_ABORT || state == NLS_STATE_QUIT
                || state == NLS_STATE_FORCE_QUIT) {
                return scalarArrayOfToArrayOfVector(ArrayOf::empty());
            }
        } catch (const NelsonException&) {
            if (profileThisCall && tic != 0) {
                internalProfileFunction stack
                    = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
                Profiler::getInstance()->toc(tic, stack);
            }
            eval->callstack.popDebug();
            throw;
        }
        for (size_t k = 0; k < outputs.size(); ++k) {
            outputs[k].name("");
        }
        return outputs;
    }

    Context* context = eval->getContext();
    Scope* callerScope = this->nestedFunction ? context->getCurrentScope() : nullptr;
    stringVector callerVariableNames;
    // The input and output arguments of a nested function are local to it: a
    // parent variable that shares a name with one of them must neither seed the
    // parameter on entry nor be overwritten by it on exit. Only genuinely shared
    // variables (used in both scopes and not a parameter here) cross the boundary.
    std::unordered_set<std::string> nestedLocalParameterNames;
    if (callerScope != nullptr) {
        callerScope->getVariablesList(false, callerVariableNames);
        for (const auto& argName : arguments) {
            nestedLocalParameterNames.insert(argName);
        }
        for (const auto& retName : returnVals) {
            nestedLocalParameterNames.insert(retName);
        }
    }
    context->pushScope(this->getName(), this->getFilename());
    std::vector<LocalFunctionBinding> localFunctionBindings;

    const std::string& filenameUtf8 = this->getFilenameUtf8();
    eval->callstack.pushDebug(filenameUtf8, getCompleteName());

    bool scopeActive = true;
    const auto releaseScope = [&]() {
        if (scopeActive) {
            restoreLocalFunctions(context, localFunctionBindings);
            context->popScope();
            scopeActive = false;
            eval->callstack.popDebug();
        }
    };
    uint64 tic = 0;
    const bool profileThisCall = (recursionDepth == 1) || this->localFunction;
    try {
        // Only insert local functions on first entry (they don't change during recursion).
        if (recursionDepth == 1) {
            localFunctionBindings = insertLocalFunctions(context);
        } else if (this->nextFunction != nullptr || this->prevFunction != nullptr) {
            localFunctionBindings = insertLocalFunctions(context);
        }
        if (callerScope != nullptr) {
            for (const auto& name : callerVariableNames) {
                if (nestedLocalParameterNames.count(name) != 0) {
                    continue;
                }
                ArrayOf value;
                if (callerScope->lookupVariable(name, value)) {
                    context->insertVariableLocally(name, value);
                }
            }
        }
        setInputArgumentNames(context, inputs);
        if (inputArgCount() != -1) {
            if (inputs.size() > arguments.size()) {
                Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
            }
            context->getCurrentScope()->setNargIn(
                static_cast<int>(std::min(inputs.size(), arguments.size())));
        } else {
            context->getCurrentScope()->setNargIn(static_cast<int>(inputs.size()));
        }
        context->getCurrentScope()->setNargOut(nargout);

        // Only profile the outermost frame of a recursive chain to reduce overhead.
        if (profileThisCall) {
            tic = Profiler::getInstance()->tic();
        }
        outputs = BytecodeVM::executeFunction(eval, *bytecodeChunk, inputs, nargout);
        if (profileThisCall && tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            releaseScope();
            return scalarArrayOfToArrayOfVector(ArrayOf::empty());
        }
        if (state == NLS_STATE_QUIT || state == NLS_STATE_FORCE_QUIT) {
            releaseScope();
            return scalarArrayOfToArrayOfVector(ArrayOf::empty());
        }

        runOnCleanupTasks(eval, context->getCurrentScope()->takeCleanupTasks());
        if (callerScope != nullptr) {
            for (const auto& name : callerVariableNames) {
                if (nestedLocalParameterNames.count(name) != 0) {
                    continue;
                }
                ArrayOf value;
                if (context->lookupVariableLocally(name, value)) {
                    callerScope->insertVariable(name, value);
                }
            }
        }
        releaseScope();
        // the popped scope may have held the last reference to handle objects
        FinalizePendingHandles(eval);
    } catch (...) {
        if (scopeActive) {
            try {
                runOnCleanupTasks(eval, context->getCurrentScope()->takeCleanupTasks());
            } catch (...) {
                // Preserve the original error and release this function's scope.
            }
            if (profileThisCall && tic != 0) {
                internalProfileFunction stack
                    = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
                Profiler::getInstance()->toc(tic, stack);
            }
            releaseScope();
            try {
                FinalizePendingHandles(eval);
            } catch (...) {
                // A finalizer must not replace the original evaluation error.
            }
        }
        throw;
    }
    for (size_t k = 0; k < outputs.size(); ++k) {
        outputs[k].name("");
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMScript(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    Context* context = eval->getContext();
    const std::string& filenameUtf8 = this->getFilenameUtf8();
    eval->callstack.pushDebug(filenameUtf8, this->getName(), this->directScript);

    struct RecursionGuard
    {
        int& depth;
        explicit RecursionGuard(int& d) : depth(d) { ++depth; }
        ~RecursionGuard() { --depth; }
        RecursionGuard(const RecursionGuard&) = delete;
        RecursionGuard&
        operator=(const RecursionGuard&)
            = delete;
    };
    static thread_local int recursionDepthScript = 0;
    RecursionGuard rgs(recursionDepthScript);

    context->getCurrentScope()->setInputArgumentNames(stringVector());
    context->getCurrentScope()->setNargIn(0);
    context->getCurrentScope()->setNargOut(0);

    uint64 tic = 0;
    std::vector<LocalFunctionBinding> localFunctionBindings;
    try {
        if (this->nextFunction != nullptr) {
            localFunctionBindings = insertLocalFunctions(context);
        }
        if (recursionDepthScript == 1) {
            tic = Profiler::getInstance()->tic();
        }
        if (bytecodeChunk != nullptr
            || (code != nullptr && !code->isEmpty() && code->down != nullptr)) {
            ensureBytecodeCompiled();
            BytecodeVM::executeScript(eval, *bytecodeChunk);
        }
        if (recursionDepthScript == 1 && tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            restoreLocalFunctions(context, localFunctionBindings);
            return scalarArrayOfToArrayOfVector(ArrayOf::empty());
        }
        restoreLocalFunctions(context, localFunctionBindings);
        eval->callstack.popDebug();
    } catch (const NelsonException&) {
        if (recursionDepthScript == 1 && tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        restoreLocalFunctions(context, localFunctionBindings);
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    struct ClassdefAccessGuard
    {
        Evaluator* eval = nullptr;
        bool active = false;
        bool constructing = false;
        bool savedWithOverload = true;
        ClassdefAccessGuard(Evaluator* evaluator, const std::string& className, bool isConstructor)
            : eval(evaluator)
        {
            active = eval != nullptr && !className.empty();
            if (active) {
                // builtin(...) bypasses the overloads of the call it makes only:
                // a method it reaches (builtin('subsref', ...) -> getter or
                // h.method) dispatches its own calls normally
                savedWithOverload = eval->withOverload;
                eval->withOverload = true;
                eval->pushClassdefAccessContext(className);
                constructing = isConstructor;
                if (constructing) {
                    eval->pushClassdefConstruction(className);
                }
            }
        }
        ~ClassdefAccessGuard()
        {
            if (active) {
                if (constructing) {
                    eval->popClassdefConstruction();
                }
                eval->popClassdefAccessContext();
                eval->withOverload = savedWithOverload;
            }
        }
        ClassdefAccessGuard(const ClassdefAccessGuard&) = delete;
        ClassdefAccessGuard&
        operator=(const ClassdefAccessGuard&)
            = delete;
    };
    if (classdefConstructorRestricted && eval != nullptr
        && !classdefCanCallConstructor(classdefOwnerClassName, eval->getClassdefAccessContext())) {
        Error("Nelson:interpreter:cannotAccessMethod", "Cannot access method '%s' in class '%s'.",
            classdefOwnerClassName, classdefOwnerClassName);
    }
    ClassdefAccessGuard classdefGuard(eval, classdefOwnerClassName, classdefConstructor);
    // Recursive calls skip lock/updateCode: the code cannot change mid-recursion.
    static thread_local int evaluateFunctionDepth = 0;
    if (evaluateFunctionDepth == 0) {
        updateCode();
    }
    ++evaluateFunctionDepth;
    ArrayOfVector result;
    try {
        if (isScript) {
            result = evaluateMScript(eval, inputs, nargout);
        } else {
            result = evaluateMFunction(eval, inputs, nargout);
        }
    } catch (...) {
        --evaluateFunctionDepth;
        throw;
    }
    --evaluateFunctionDepth;
    return result;
}
//=============================================================================
void
MacroFunctionDef::invalidateBytecode()
{
    bytecodeChunk.reset();
    needsContextScopeCache = -1;
    completeNameCache.clear();
}
//=============================================================================
// Collect the names declared 'global' by the top-level statements of a function
// body, so a nested function can inherit the enclosing function's globals and
// resolve a shared global instead of an unseeded local.
static void
collectTopLevelGlobals(AbstractSyntaxTreePtr body, stringVector& out)
{
    if (body == nullptr) {
        return;
    }
    for (AbstractSyntaxTreePtr statement = body->down; statement != nullptr;
        statement = statement->right) {
        // statement (OP_QSTATEMENT/OP_RSTATEMENT) -> down is the statement node;
        // a 'global'/'persistent' statement is an OP_SCALL whose down is the
        // 'global' id_node with the declared names chained on its right.
        AbstractSyntaxTreePtr node = statement->down;
        if (node != nullptr && node->opNum == OP_SCALL) {
            node = node->down;
        }
        if (node != nullptr && node->type == id_node && node->text == "global") {
            for (AbstractSyntaxTreePtr arg = node->right; arg != nullptr; arg = arg->right) {
                if (!arg->text.empty()) {
                    out.push_back(arg->text);
                }
            }
        }
    }
}
//=============================================================================
bool
MacroFunctionDef::ensureBytecodeCompiled()
{
    if (bytecodeChunk != nullptr) {
        return false;
    }
    if (code == nullptr && !isScript) {
        return false;
    }
    if (isScript) {
        bytecodeChunk = BytecodeCompiler::compileScript(code, getFilename());
    } else {
        applyArgumentsBlockLowering(this);
        stringVector inheritedGlobals;
        if (this->nestedFunction) {
            // Walk the enclosing-function chain (prevFunction is the lexical
            // parent) and inherit every global they declare.
            for (MacroFunctionDef* enclosing = this->prevFunction; enclosing != nullptr;
                enclosing = enclosing->prevFunction) {
                collectTopLevelGlobals(enclosing->code, inheritedGlobals);
                if (!enclosing->nestedFunction) {
                    break;
                }
            }
        }
        bytecodeChunk = BytecodeCompiler::compileFunction(code, getName(), getFilename(), arguments,
            returnVals, classdefOwnerClassName, inheritedGlobals);
    }
    if (bytecodeChunk != nullptr) {
        bytecodeChunk->selfFunction = this;
        bytecodeChunk->selfIsNestedMacro = this->nestedFunction;
    }
    return bytecodeChunk != nullptr;
}
//=============================================================================
bool
MacroFunctionDef::canEvaluateScopeFree(Evaluator* eval)
{
    if (this->nestedFunction || this->isScript) {
        return false;
    }
    // ensureBytecodeCompiled reports whether it *compiled* something, so it
    // returns false once the chunk is already there: test the chunk itself.
    ensureBytecodeCompiled();
    if (bytecodeChunk == nullptr) {
        return false;
    }
    // The scope-free decision resolves callee names: any function-table
    // mutation can rebind them, so the cache is keyed on the tables
    // generation. Purely static verdicts never change once computed.
    const uint64 functionTablesGeneration = FunctionsInMemory::getInstance()->generation();
    if (needsContextScopeCache < 0 || needsContextScopeGeneration != functionTablesGeneration) {
        needsContextScopeCache = bytecodeNeedsContextScope(eval, *bytecodeChunk) ? 1 : 0;
        needsContextScopeGeneration = functionTablesGeneration;
    }
    return needsContextScopeCache == 0;
}
//=============================================================================
const std::string&
MacroFunctionDef::getCompleteName()
{
    if (completeNameCache.empty()) {
        completeNameCache = computeCompleteName();
    }
    return completeNameCache;
}
//=============================================================================
std::string
MacroFunctionDef::computeCompleteName()
{
    if (this->localFunction) {
        MacroFunctionDef* root = this;
        while (root->prevFunction != nullptr) {
            root = root->prevFunction;
        }
        if (!this->nestedFunction && !root->getIsScript()) {
            return this->getName();
        }
        std::vector<std::string> names;
        names.push_back(this->getName());
        MacroFunctionDef* pF = this->prevFunction;
        while (pF != nullptr) {
            names.push_back(pF->getName());
            if (!pF->nestedFunction && !pF->getIsScript()) {
                break;
            }
            pF = pF->prevFunction;
        }
        std::string completeName;
        for (auto it = names.rbegin(); it != names.rend(); ++it) {
            if (!completeName.empty()) {
                completeName += ">";
            }
            completeName += *it;
        }
        return completeName;
    }
    return this->getName();
}
//=============================================================================
std::vector<MacroFunctionDef::LocalFunctionBinding>
MacroFunctionDef::insertLocalFunctions(Context* context)
{
    std::vector<LocalFunctionBinding> localFunctionBindings;
    MacroFunctionDef* cp = this;
    while (cp->prevFunction != nullptr) {
        cp = cp->prevFunction;
    }
    cp = cp->nextFunction;
    Scope* currentScope = context->getCurrentScope();
    const bool canRestorePreviousFunction
        = currentScope != nullptr && currentScope->getName() != "base";
    while (cp != nullptr) {
        LocalFunctionBinding binding;
        binding.name = cp->getName();
        FunctionDefPtr previousFunction = nullptr;
        if (currentScope != nullptr && currentScope->lookupFunction(binding.name, previousFunction)
            && canRestorePreviousFunction) {
            binding.previousFunction = previousFunction;
            binding.restorePreviousFunction = true;
        }
        localFunctionBindings.push_back(binding);
        context->insertMacroFunctionLocally((FunctionDefPtr)cp);
        cp = cp->nextFunction;
    }
    return localFunctionBindings;
}
//=============================================================================
void
MacroFunctionDef::restoreLocalFunctions(
    Context* context, const std::vector<LocalFunctionBinding>& localFunctionBindings)
{
    for (auto it = localFunctionBindings.rbegin(); it != localFunctionBindings.rend(); ++it) {
        if (it->restorePreviousFunction) {
            context->getCurrentScope()->insertMacroFunctionLocally(it->previousFunction);
        } else {
            context->removeMacroFunctionLocally(it->name);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
