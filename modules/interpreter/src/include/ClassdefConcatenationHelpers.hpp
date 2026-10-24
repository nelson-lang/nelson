//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include <cctype>
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefMixinNames.hpp"
#include "ConcatenateDimensions.hpp"
#include "Error.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "OverloadName.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::ClassdefConcatenationHelpers {
//=============================================================================
inline std::string
lowerString(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return value;
}
//=============================================================================
inline std::string
classNameLeaf(const std::string& className)
{
    size_t pos = className.find_last_of('.');
    if (pos == std::string::npos || pos + 1 >= className.size()) {
        return className;
    }
    return className.substr(pos + 1);
}
//=============================================================================
inline bool
sameClassName(const std::string& left, const std::string& right)
{
    const std::string normalizedLeft = lowerString(left);
    const std::string normalizedRight = lowerString(right);
    return normalizedLeft == normalizedRight || lowerString(classNameLeaf(left)) == normalizedRight
        || normalizedLeft == lowerString(classNameLeaf(right));
}
//=============================================================================
inline bool
inferiorListContains(const std::string& className, const std::string& inferiorClassName)
{
    for (const auto& current : classdefInferiorClasses(className)) {
        if (sameClassName(current, inferiorClassName)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
inline bool
classdefDominates(const std::string& candidateClassName, const std::string& currentClassName)
{
    if (sameClassName(candidateClassName, currentClassName)) {
        return false;
    }
    if (inferiorListContains(candidateClassName, currentClassName)) {
        return true;
    }
    if (inferiorListContains(currentClassName, candidateClassName)) {
        return false;
    }
    return false;
}
//=============================================================================
inline std::string
dominantClassName(const ArrayOfVector& args)
{
    std::string dominantClass;
    for (const auto& arg : args) {
        if (!arg.isClassdefType()) {
            continue;
        }
        const std::string currentClass = arg.classType();
        if (dominantClass.empty() || classdefDominates(currentClass, dominantClass)) {
            dominantClass = currentClass;
        }
    }
    return dominantClass;
}
//=============================================================================
inline ArrayOf
evaluateConversionFunction(Evaluator* eval, const std::string& functionName, const ArrayOf& value)
{
    FunctionDefPtr functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        RaiseError("Function not found: " + functionName, "Nelson:interpreter:functionNotFound");
    }
    ArrayOfVector inputs;
    inputs.push_back(value);
    ArrayOfVector outputs = functionDef->evaluateFunction(eval, inputs, 1);
    if (outputs.empty()) {
        Error("Nelson:interpreter:conversionFailed", "Conversion function did not return a value.");
    }
    return outputs[0];
}
//=============================================================================
// The class constructor converts the operand; its failure names the conversion.
inline ArrayOf
convertWithConstructor(Evaluator* eval, const ArrayOf& value, const std::string& targetClass)
{
    try {
        return evaluateConversionFunction(eval, targetClass, value);
    } catch (const NelsonException& e) {
        // Nelson:UnableToConvert already carries another template in the
        // identifier-keyed catalog: the message is resolved text-keyed. Class
        // names are ASCII identifiers.
        const std::string from = ClassName(value);
        RaiseError(
            formatI18nMessage(_W("The following error occurred converting from %s to %s:\n%s"),
                std::wstring(from.begin(), from.end()),
                std::wstring(targetClass.begin(), targetClass.end()), e.message()),
            L"Nelson:UnableToConvert");
    }
    return {};
}
//=============================================================================
// A string operand goes through the conversion even when empty; the other
// empty operands are left to the concatenation, which drops them.
inline bool
needsObjectConversion(const ArrayOf& value)
{
    return !value.isEmpty(false) || value.isStringArray();
}
//=============================================================================
inline ArrayOf
emptyObjectLike(const ArrayOf& object, Dimensions dims)
{
    if (object.isHandle()) {
        return ArrayOf::emptyHandle(ClassName(object), dims);
    }
    ArrayOf result = ArrayOf::emptyClassdef(object.classType(), object.fieldNames(), dims);
    result.setClassdefLayout(object.classdefLayout());
    return result;
}
//=============================================================================
inline bool
isObjectOfClass(const ArrayOf& value, const ArrayOf& object)
{
    if (object.isHandle()) {
        return value.isHandle();
    }
    return value.isClassdefType() && sameClassName(value.classType(), object.classType());
}
//=============================================================================
// Once converted, an empty operand of another class stands for an empty object
// of its size. With dropEmpties (horzcat, vertcat), next to a nonempty operand
// a 0x0 operand is dropped, and so is every 2-D empty operand unless an operand
// is N-D; the others are left to the dimension rules
// ([zeros(0, 1), repmat(o, 1, 1, 2)] is an error).
inline ArrayOfVector
shapeObjectOperands(const ArrayOfVector& converted, const ArrayOf& object, bool dropEmpties)
{
    bool hasNonEmpty = false;
    bool hasNdOperand = false;
    for (const auto& arg : converted) {
        hasNonEmpty = hasNonEmpty || !arg.isEmpty(false);
        hasNdOperand = hasNdOperand || !arg.is2D();
    }
    ArrayOfVector shaped;
    shaped.reserve(converted.size());
    for (const auto& arg : converted) {
        if (dropEmpties && hasNonEmpty && arg.isEmpty(false) && arg.is2D()
            && (!hasNdOperand || isZeroByZeroDimensions(arg.dimensions()))) {
            continue;
        }
        const bool keep = !arg.isEmpty(false) || isObjectOfClass(arg, object);
        shaped.push_back(keep ? arg : emptyObjectLike(object, arg.dimensions()));
    }
    return shaped;
}
//=============================================================================
inline ArrayOf
convertToDominantClass(Evaluator* eval, const ArrayOf& value, const std::string& targetClass)
{
    if (!needsObjectConversion(value)) {
        return value;
    }
    if (value.isClassdefType() && sameClassName(value.classType(), targetClass)) {
        return value;
    }
    if (value.isClassdefType()) {
        std::string functionName;
        if (classdefResolveMethodFunction(
                value.classType(), targetClass, functionName, eval->getClassdefAccessContext())) {
            return evaluateConversionFunction(eval, functionName, value);
        }
    }
    if (!classdefLoadClass(targetClass)) {
        RaiseError("Class not found: " + targetClass, "Nelson:interpreter:classNotFound");
    }
    return convertWithConstructor(eval, value, targetClass);
}
//=============================================================================
// The builtin type an enumeration derives from (logical for OnOffSwitchState),
// or empty when the class is not such an enumeration. The name list mirrors
// isEnumerationBaseType() in ClassdefParserAttributes; kept local so this
// header stays free of that non-exported symbol across module boundaries.
inline std::string
enumerationBuiltinBase(const std::string& className)
{
    if (classdefEnumerations(className).empty()) {
        return {};
    }
    static const char* const baseNames[] = { "logical", "double", "single", "int8", "uint8",
        "int16", "uint16", "int32", "uint32", "int64", "uint64" };
    for (const auto& superclass : classdefSuperclasses(className)) {
        const std::string name = lowerString(superclass);
        for (const char* baseName : baseNames) {
            if (name == baseName) {
                return name;
            }
        }
    }
    return {};
}
//=============================================================================
inline bool
isBuiltinBaseEnumerationOperand(const ArrayOf& value)
{
    return value.isClassdefType() && !enumerationBuiltinBase(value.classType()).empty();
}
//=============================================================================
// An enumeration whose base is a builtin type stands in for that base
// ('on'/'off' for OnOffSwitchState). Concatenated with text it behaves like its
// char/string form instead of dominating as a classdef, so
// ['delete:' h.BeingDeleted] yields 'delete:on'. Each enumeration operand is
// lowered to char (or string when a string operand is present) and the builtin
// type rules finish the concatenation. Numeric-only mixes are left untouched.
inline bool
lowerBuiltinBaseEnumerationOperands(
    Evaluator* eval, const ArrayOfVector& args, ArrayOfVector& loweredArgs)
{
    loweredArgs.clear();
    bool hasEnumeration = false;
    bool hasChar = false;
    bool hasString = false;
    for (const auto& arg : args) {
        if (isBuiltinBaseEnumerationOperand(arg)) {
            hasEnumeration = true;
        } else if (arg.isCharacterArray()) {
            hasChar = true;
        } else if (arg.isStringArray()) {
            hasString = true;
        }
    }
    if (!hasEnumeration || !(hasChar || hasString)) {
        return false;
    }
    const std::string target = hasChar ? std::string("char") : std::string("string");
    ArrayOfVector lowered;
    lowered.reserve(args.size());
    for (const auto& arg : args) {
        lowered.push_back(
            isBuiltinBaseEnumerationOperand(arg) ? convertToDominantClass(eval, arg, target) : arg);
    }
    loweredArgs = lowered;
    return true;
}
//=============================================================================
inline std::string
heterogeneousMixinClassName()
{
    return NLS_MIXIN_HETEROGENEOUS;
}
//=============================================================================
inline stringVector
classAncestry(const std::string& className)
{
    stringVector order;
    stringVector queue { className };
    while (!queue.empty()) {
        const std::string current = queue.front();
        queue.erase(queue.begin());
        bool known = false;
        for (const auto& name : order) {
            if (sameClassName(name, current)) {
                known = true;
                break;
            }
        }
        if (known) {
            continue;
        }
        order.push_back(current);
        for (const auto& superclass : classdefSuperclasses(current)) {
            queue.push_back(superclass);
        }
    }
    return order;
}
//=============================================================================
inline std::string
nearestCommonHeterogeneousRoot(const ArrayOfVector& args)
{
    stringVector classes;
    for (const auto& arg : args) {
        if (!arg.isClassdefType() || arg.isEmpty(false)) {
            continue;
        }
        const std::string currentClass = arg.classType();
        bool known = false;
        for (const auto& name : classes) {
            if (sameClassName(name, currentClass)) {
                known = true;
                break;
            }
        }
        if (!known) {
            classes.push_back(currentClass);
        }
    }
    if (classes.size() < 2) {
        return {};
    }
    for (const auto& candidate : classAncestry(classes[0])) {
        if (sameClassName(candidate, heterogeneousMixinClassName())
            || !classdefIsA(candidate, heterogeneousMixinClassName())) {
            continue;
        }
        bool commonRoot = true;
        for (const auto& currentClass : classes) {
            if (!sameClassName(currentClass, candidate) && !classdefIsA(currentClass, candidate)) {
                commonRoot = false;
                break;
            }
        }
        if (commonRoot) {
            return candidate;
        }
    }
    return {};
}
//=============================================================================
inline ArrayOf
projectToHeterogeneousRoot(const ArrayOf& value, const std::string& rootClass)
{
    if (!value.isClassdefType() || sameClassName(value.classType(), rootClass)) {
        return value;
    }
    ClassdefLayoutPtr layout = classdefLayout(rootClass);
    stringVector fields = layout == nullptr ? stringVector() : layout->storagePropertyNames;
    Dimensions dims = value.dimensions();
    if (fields.empty()) {
        void* data
            = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fields, true);
        ArrayOf projected(NLS_CLASSDEF_ARRAY, dims, data, false, fields);
        projected.setClassType(rootClass);
        projected.setClassdefLayout(layout);
        return projected;
    }
    std::vector<ArrayOfVector> fieldLists;
    fieldLists.reserve(fields.size());
    for (const auto& field : fields) {
        fieldLists.push_back(value.fieldAsList(field));
    }
    void* data = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fields, true);
    ArrayOf projected(NLS_CLASSDEF_ARRAY, dims, data, false, fields);
    projected.setClassType(rootClass);
    projected.setClassdefLayout(layout);
    for (indexType k = 0; k < static_cast<indexType>(dims.elementCount()); ++k) {
        ArrayOfVector scalarValues;
        scalarValues.reserve(fields.size());
        for (size_t f = 0; f < fields.size(); ++f) {
            scalarValues.push_back(static_cast<size_t>(k) < fieldLists[f].size()
                    ? fieldLists[f][k]
                    : ArrayOf::empty());
        }
        ArrayOf scalarElement = ArrayOf::classdefArray(rootClass, fields, scalarValues);
        scalarElement.copyElements(0, data, k, 1);
    }
    return projected;
}
//=============================================================================
// nelson.mixin.Scalar: concatenating such objects into a non-scalar array is an error.
inline void
rejectScalarClassConcatenation(const ArrayOfVector& args)
{
    std::string scalarClass;
    for (const auto& a : args) {
        if (a.isClassType() && classdefIsA(a.classType(), NLS_MIXIN_SCALAR)) {
            scalarClass = a.classType();
            break;
        }
    }
    if (scalarClass.empty()) {
        return;
    }
    indexType total = 0;
    for (const auto& a : args) {
        total += a.elementCount();
    }
    if (total > 1) {
        RaiseError("Concatenating '" + scalarClass + "' objects is not supported because '"
                + scalarClass + "' objects can only be scalar.",
            "Nelson:class:concatenationScalar");
    }
}
//=============================================================================
// The dominant object class, even from an empty object, decides the
// concatenation: nothing to prepare when every operand is an object of that
// class and they are all empty or all nonempty.
inline bool
prepareArguments(Evaluator* eval, const ArrayOfVector& args, std::string& typeName,
    ArrayOfVector& convertedArgs, bool dropEmpties = true)
{
    typeName.clear();
    convertedArgs.clear();
    const std::string dominantClass = dominantClassName(args);
    if (dominantClass.empty()) {
        return false;
    }
    bool needsConversion = false;
    bool hasEmpty = false;
    bool hasNonEmpty = false;
    for (const auto& arg : args) {
        needsConversion = needsConversion || !arg.isClassdefType()
            || !sameClassName(arg.classType(), dominantClass);
        hasEmpty = hasEmpty || arg.isEmpty(false);
        hasNonEmpty = hasNonEmpty || !arg.isEmpty(false);
    }
    if (!needsConversion && !(hasEmpty && hasNonEmpty)) {
        return false;
    }
    // Distinct classes sharing a heterogeneous root concatenate as the nearest
    // common root class instead of converting to a dominant class.
    const std::string heterogeneousRoot = nearestCommonHeterogeneousRoot(args);
    typeName = heterogeneousRoot.empty() ? dominantClass : heterogeneousRoot;
    ArrayOfVector converted;
    converted.reserve(args.size());
    for (const auto& arg : args) {
        converted.push_back(heterogeneousRoot.empty()
                ? convertToDominantClass(eval, arg, dominantClass)
                : projectToHeterogeneousRoot(arg, heterogeneousRoot));
    }
    convertedArgs = converted;
    for (const auto& arg : converted) {
        if (arg.isClassdefType() && sameClassName(arg.classType(), typeName)) {
            convertedArgs = shapeObjectOperands(converted, arg, dropEmpties);
            break;
        }
    }
    return true;
}
//=============================================================================
// A concatenation method of the class or of a superclass, the native handle
// concatenation excepted.
inline bool
hasConcatenationMethod(
    Evaluator* eval, const std::string& className, const std::string& functionName)
{
    std::string methodName;
    return classdefResolveMethodFunction(
               className, functionName, methodName, eval->getClassdefAccessContext())
        && methodName != getOverloadFunctionName(NLS_HANDLE_STR, functionName);
}
//=============================================================================
inline std::string
classdefHandleClassName(const ArrayOfVector& args)
{
    for (const auto& arg : args) {
        if (arg.isHandle()) {
            const std::string className = ClassName(arg);
            return classdefIsHandleClass(className) ? className : std::string();
        }
    }
    return {};
}
//=============================================================================
// Handles of a classdef class concatenate only with handles: without a
// concatenation method of its own, the class constructor converts every other
// operand (a string even when empty).
inline bool
prepareHandleArguments(Evaluator* eval, const ArrayOfVector& args, const std::string& functionName,
    ArrayOfVector& convertedArgs, bool dropEmpties = true)
{
    convertedArgs.clear();
    bool hasOtherOperand = false;
    for (const auto& arg : args) {
        hasOtherOperand = hasOtherOperand || !arg.isHandle();
    }
    if (!hasOtherOperand) {
        return false;
    }
    const std::string handleClass = classdefHandleClassName(args);
    if (handleClass.empty() || hasConcatenationMethod(eval, handleClass, functionName)) {
        return false;
    }
    ArrayOfVector converted;
    converted.reserve(args.size());
    const ArrayOf* object = nullptr;
    for (const auto& arg : args) {
        converted.push_back(arg.isHandle() ? arg : convertToDominantClass(eval, arg, handleClass));
        if (object == nullptr && arg.isHandle()) {
            object = &arg;
        }
    }
    convertedArgs = shapeObjectOperands(converted, *object, dropEmpties);
    return true;
}
//=============================================================================
// cat along a dimension above 2 drops only the 0x0 operands before
// concatenating with vertcat: the object operands convert the others first,
// and the other empty operands keep their size as empty objects.
inline bool
prepareCatArguments(Evaluator* eval, const ArrayOfVector& args, ArrayOfVector& convertedArgs)
{
    const std::string functionName = "vertcat";
    if (prepareHandleArguments(eval, args, functionName, convertedArgs, false)) {
        return true;
    }
    const std::string dominantClass = dominantClassName(args);
    if (dominantClass.empty() || hasConcatenationMethod(eval, dominantClass, functionName)) {
        return false;
    }
    std::string typeName;
    return prepareArguments(eval, args, typeName, convertedArgs, false);
}
//=============================================================================
} // namespace Nelson::ClassdefConcatenationHelpers
//=============================================================================
