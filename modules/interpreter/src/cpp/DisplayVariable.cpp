//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "DisplayVariable.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefMixinNames.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadName.hpp"
#include "OverloadRequired.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "NelsonException.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
objectCompactDisplayHookImpl(const ArrayOf& A, std::wstring& representation);
static bool
objectDimensionsHookImpl(const ArrayOf& A, Dimensions& dims);
static bool
objectElementTextsHookImpl(const ArrayOf& A, wstringVector& texts);
static void
ensureObjectCompactDisplayHookRegistered();
//=============================================================================
static bool
tryClassdefDisplayOverload(
    Evaluator* eval, const ArrayOf& A, const ArrayOfVector& args, const std::string& functionName)
{
    if (!isOverloadObjectType(A.dataClass())) {
        return false;
    }
    std::string currentType = ClassName(A);
    if (currentType.empty() || !classdefLoadClass(currentType)) {
        return false;
    }
    std::string overloadName = getOverloadFunctionName(currentType, functionName);
    if (!classdefResolveMethodFunction(
            currentType, functionName, overloadName, eval->getClassdefAccessContext())) {
        return false;
    }
    FunctionDef* funcDef = nullptr;
    if (!eval->getContext()->lookupFunction(overloadName, funcDef) || funcDef == nullptr) {
        return false;
    }
    funcDef->evaluateFunction(eval, args, 0);
    return true;
}
//=============================================================================
void
Evaluator::display(
    const ArrayOf& A, const std::string& name, bool asDispBuiltin, bool withProfiling)
{
    ensureObjectCompactDisplayHookRegistered();
    bool wasFound = false;

    std::string functionName = asDispBuiltin ? "disp" : "display";
    ArrayOfVector args;
    args << A;
    if (!name.empty()) {
        args << ArrayOf::characterArray(name);
    }
    if (tryClassdefDisplayOverload(this, A, args, functionName)) {
        return;
    }
    callOverloadedFunction(this,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args, functionName,
        ClassName(A), A.dataClass(), wasFound, 0);
    if (wasFound) {
        return;
    }
    uint64 ticProfile = withProfiling ? Profiler::getInstance()->tic() : 0U;

    bool needToOverload = false;
    DisplayVariable(this->getID(), this->getInterface(), A, name, asDispBuiltin, needToOverload);
    if (ticProfile != 0U) {
        internalProfileFunction stack = computeProfileStack(this, functionName, L"evaluator");
        Profiler::getInstance()->toc(ticProfile, stack);
    }
    if (needToOverload) {
        callOverloadedFunction(this, NLS_OVERLOAD_OBJECT_TYPES_ONLY, args, functionName,
            ClassName(A), A.dataClass(), wasFound, 0);
        if (!wasFound) {
            OverloadRequired(functionName);
        }
    }
}
//=============================================================================
// Container compact-display hook: a scalar function handle shown inside a cell/struct
// is its definition text (@sin). A nelson.mixin.CustomCompactDisplayProvider object calls
// its compactRepresentationForSingleLine method (if overridden) and uses the returned char
// text. Guarded against re-entrancy and errors; only fires for provider classes, so other
// objects are unaffected.
static bool
objectCompactDisplayHookImpl(const ArrayOf& A, std::wstring& representation)
{
    if (A.isFunctionHandle()) {
        auto* definition
            = reinterpret_cast<AnonymousMacroFunctionDef*>(A.asFunctionHandle().anonymousHandle);
        if (!A.isScalar() || definition == nullptr) {
            return false;
        }
        representation = utf8_to_wstring(definition->getDefinition());
        return true;
    }
    static thread_local bool inHook = false;
    if (inHook) {
        return false;
    }
    std::string className = ClassName(A);
    if (className.empty() || !classdefIsA(className, NLS_MIXIN_CUSTOM_COMPACT_DISPLAY_PROVIDER)) {
        return false;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval == nullptr || eval->getContext() == nullptr) {
        return false;
    }
    std::string overload = getOverloadFunctionName(className, "__nls_compact_render__");
    FunctionDef* funcDef = nullptr;
    if (!eval->getContext()->lookupFunction(overload, funcDef) || funcDef == nullptr) {
        return false;
    }
    double width = 80.0;
    if (eval->getInterface() != nullptr) {
        width = static_cast<double>(eval->getInterface()->getTerminalWidth());
    }
    inHook = true;
    ArrayOfVector out;
    try {
        ArrayOfVector in;
        in << A;
        in << ArrayOf::doubleScalar(width);
        out = funcDef->evaluateFunction(eval, in, 1);
    } catch (const NelsonException&) {
        inHook = false;
        return false;
    }
    inHook = false;
    if (out.empty() || !out[0].isRowVectorCharacterArray()) {
        return false;
    }
    representation = utf8_to_wstring(out[0].asUtf8String());
    return true;
}
//=============================================================================
// Container size summary of an object: the dimensions its class size method reports
// (duration, datetime, categorical, table keep their data in hidden properties).
static bool
objectDimensionsHookImpl(const ArrayOf& A, Dimensions& dims)
{
    static thread_local bool inHook = false;
    if (inHook) {
        return false;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval == nullptr || eval->getContext() == nullptr) {
        return false;
    }
    inHook = true;
    ArrayOfVector sizes;
    bool found = false;
    try {
        found = eval->bytecodeInvokeObjectMethodIfExists(A, "size", {}, 1, sizes);
    } catch (const NelsonException&) {
        found = false;
    }
    inHook = false;
    if (!found || sizes.empty() || !sizes[0].isNumeric()) {
        return false;
    }
    std::vector<double> values = sizes[0].asDoubleVector();
    if (values.size() < 2) {
        return false;
    }
    std::vector<indexType> lengths;
    lengths.reserve(values.size());
    for (double value : values) {
        lengths.push_back(static_cast<indexType>(value));
    }
    dims = Dimensions(lengths);
    return true;
}
//=============================================================================
// Container text of an object row: the element texts its class returns from the hidden
// compactElementText method (duration, datetime, calendarDuration, categorical).
static bool
objectElementTextsHookImpl(const ArrayOf& A, wstringVector& texts)
{
    static thread_local bool inHook = false;
    if (inHook || !A.isClassType()) {
        return false;
    }
    const std::string methodName = "compactElementText";
    std::string className = A.classType();
    if (!classdefLoadClass(className) || !classdefHasMethodDefinition(className, methodName)) {
        return false;
    }
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (eval == nullptr || eval->getContext() == nullptr) {
        return false;
    }
    inHook = true;
    ArrayOfVector out;
    bool found = false;
    try {
        found = eval->bytecodeInvokeObjectMethodIfExists(A, methodName, {}, 1, out);
    } catch (const NelsonException&) {
        found = false;
    }
    inHook = false;
    if (!found || out.empty()
        || !(out[0].isStringArray() || out[0].isCellArrayOfCharacterVectors())) {
        return false;
    }
    try {
        texts = out[0].asWideStringVector(false);
    } catch (const NelsonException&) {
        return false;
    }
    return true;
}
//=============================================================================
static void
ensureObjectCompactDisplayHookRegistered()
{
    static bool registered = []() {
        setObjectCompactDisplayHook(objectCompactDisplayHookImpl);
        setObjectDimensionsHook(objectDimensionsHookImpl);
        setObjectElementTextsHook(objectElementTextsHookImpl);
        return true;
    }();
    (void)registered;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
