//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// classdef property access helpers. They resolve constant, dependent and stored
// properties to comma-list values on the dot-access and classdef-member call
// paths (cold relative to the scalar dispatch loop).
//=============================================================================
#include <iomanip>
#include <sstream>
#include <string>
#include "BytecodeVMInternal.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefMixinNames.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "ContainersMapObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "FunctionsInMemory.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static std::string
containersMapCharField(const std::string& key);
//=============================================================================
bool
splitClassdefMemberSpec(const std::string& spec, std::string& className, std::string& memberName)
{
    size_t pos = spec.find_last_of('.');
    if (pos == std::string::npos || pos == 0 || pos + 1 >= spec.size()) {
        return false;
    }
    className = spec.substr(0, pos);
    memberName = spec.substr(pos + 1);
    return true;
}
//=============================================================================
bool
isClassdefStaticMethodName(const std::string& name, const std::string& callerClassName)
{
    // "Class.method" resolving to itself as a static method of a loaded class
    std::string className;
    std::string memberName;
    std::string functionName;
    return splitClassdefMemberSpec(name, className, memberName) && classdefLoadClass(className)
        && classdefResolveStaticMethodFunction(className, memberName, functionName, callerClassName)
        && functionName == name;
}
//=============================================================================
void
checkClassdefPropertyGet(Evaluator* eval, const ArrayOf& base, const std::string& field)
{
    if (!base.isClassType()) {
        return;
    }
    const std::string className = base.classType();

    if (!classdefLoadClass(className)) {
        return;
    }
    if (!classdefHasProperty(className, field)) {
        RaiseError(formatI18nMessage(_(BYTECODE_VM_CLASSDEF_ERROR_NO_SUCH_PROPERTY), field),
            "Nelson:interpreter:noSuchProperty");
    }
    if (!classdefCanGetProperty(
            className, field, eval == nullptr ? std::string() : eval->getClassdefAccessContext())) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
}
//=============================================================================
ArrayOfVector
getClassdefConstantFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field)
{
    const std::string className = base.classType();

    if (!classdefHasConstantProperty(className, field)) {
        return {};
    }
    checkClassdefPropertyGet(eval, base, field);
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
    std::string functionName;
    if (!classdefResolveConstantPropertyFunction(
            className, field, functionName, eval->getClassdefAccessContext())) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
    FunctionDef* functionDef = nullptr;
    if (!lookupOverloadFunctionCached(eval->getContext(), functionName, functionDef)
        || functionDef == nullptr) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
    ArrayOfVector result = functionDef->evaluateFunction(eval, {}, 1);
    ArrayOf value = result.empty() ? ArrayOf::empty() : result[0];
    ArrayOfVector values;
    const indexType elementCount = base.elementCount();
    values.reserve(static_cast<size_t>(elementCount));
    for (indexType k = 0; k < elementCount; ++k) {
        values << value;
    }
    return values;
}
//=============================================================================
ArrayOfVector
getClassdefDependentFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field)
{
    const std::string className = base.classType();

    if (!classdefHasDependentProperty(className, field)) {
        return {};
    }
    checkClassdefPropertyGet(eval, base, field);
    if (eval == nullptr || eval->getContext() == nullptr) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
    std::string functionName;
    if (!classdefResolvePropertyAccessorFunction(
            className, field, BYTECODE_VM_CLASSDEF_ACCESSOR_GET, functionName)) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }
    FunctionDef* functionDef = nullptr;
    if (!lookupOverloadFunctionCached(eval->getContext(), functionName, functionDef)
        || functionDef == nullptr) {
        RaiseError(_(BYTECODE_VM_CLASSDEF_ERROR_CANNOT_GET_PROPERTY) + field,
            "Nelson:interpreter:cannotGetProperty");
    }

    ArrayOfVector values;
    const indexType elementCount = base.elementCount();
    values.reserve(static_cast<size_t>(elementCount));
    for (indexType k = 0; k < elementCount; ++k) {
        ArrayOf object = base;
        if (elementCount != 1) {
            ArrayOf mutableBase = base;
            ArrayOf index = ArrayOf::doubleScalar(static_cast<double>(k + 1));
            object = mutableBase.vectorSubset(index);
        }
        ArrayOfVector inputs;
        inputs << object;
        ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
        values << (result.empty() ? ArrayOf::empty() : result[0]);
    }
    return values;
}
//=============================================================================
ArrayOfVector
getClassdefFieldAsList(Evaluator* eval, const ArrayOf& base, const std::string& field)
{
    // a plain stored property cannot be constant, dependent nor access
    // restricted: read the stored field directly
    if (base.isClassType() && base.isScalar() && classdefLoadClass(base.classType())
        && classdefPlainStoredProperty(base.classType(), field, false) != nullptr) {
        int64 fieldIndex = base.fieldIndex(field);
        if (fieldIndex >= 0) {
            const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
            ArrayOf value = elements[static_cast<indexType>(fieldIndex)];
            value.name("");
            ArrayOfVector single;
            single << value;
            return single;
        }
    }
    checkClassdefPropertyGet(eval, base, field);
    if (base.isClassType()) {
        ArrayOfVector constantValues = getClassdefConstantFieldAsList(eval, base, field);
        if (!constantValues.empty() || classdefHasConstantProperty(base.classType(), field)) {
            return constantValues;
        }
        ArrayOfVector dependentValues = getClassdefDependentFieldAsList(eval, base, field);
        if (!dependentValues.empty() || classdefHasDependentProperty(base.classType(), field)) {
            return dependentValues;
        }
    }
    return base.fieldAsList(field);
}
//=============================================================================
bool
tryReadClassdefPlainProperty(
    Evaluator* eval, const ArrayOf& base, const std::string& field, ArrayOf& value)
{
    if (!base.isScalar() || !classdefLoadClass(base.classType())) {
        return false;
    }
    // an overloaded subsref owns obj.field outside the methods of the class
    bool subsrefOverloaded = false;
    if (classdefPlainStoredProperty(base.classType(), field, false, subsrefOverloaded) == nullptr
        || eval->isOverloadedDotAccess(base.classType(), subsrefOverloaded)) {
        return false;
    }
    int64 fieldIndex = base.fieldIndex(field);
    if (fieldIndex < 0) {
        return false;
    }
    const auto* elements = static_cast<const ArrayOf*>(base.dataPointer());
    value = elements[static_cast<indexType>(fieldIndex)];
    value.name("");
    return true;
}
//=============================================================================
// Build a 1xN nelson.indexing.IndexingOperation array from a collected chain,
// then dispatch the whole chain to the Redefines* object's hook once, so the
// hook receives the full operation list (numel>1).
// Returns false when the class does not provide the matching hook.
bool
dispatchRedefinesWholeChain(Evaluator* eval, const ArrayOf& base, const stringVector& subtypes,
    const ArrayOfVector& subsindices, ArrayOf& result)
{
    if (eval == nullptr || eval->getContext() == nullptr || subtypes.empty()) {
        return false;
    }
    Context* ctx = eval->getContext();
    FunctionDef* constructor = nullptr;
    if (!ctx->lookupFunction(NLS_INDEXING_OPERATION, constructor) || constructor == nullptr) {
        return false;
    }
    ArrayOfVector ops;
    for (size_t i = 0; i < subtypes.size(); ++i) {
        const std::string typeName = subtypes[i] == "()" ? "Paren"
            : subtypes[i] == "{}"                        ? "Brace"
                                                         : "Dot";
        ArrayOfVector ctorArgs;
        ctorArgs.push_back(ArrayOf::characterArray(typeName));
        if (subtypes[i] == ".") {
            ctorArgs.push_back(ArrayOf::emptyCell(Dimensions(0, 0)));
            ctorArgs.push_back(subsindices[i]);
        } else {
            ctorArgs.push_back(subsindices[i]);
        }
        ArrayOfVector made = constructor->evaluateFunction(eval, ctorArgs, 1);
        ops.push_back(made.empty() ? ArrayOf::empty() : made[0]);
    }
    ArrayOf opsArray;
    if (ops.size() == 1) {
        opsArray = ops[0];
    } else {
        FunctionDef* horzcat = nullptr;
        if (!ctx->lookupFunction("horzcat", horzcat) || horzcat == nullptr) {
            return false;
        }
        ArrayOfVector cat = horzcat->evaluateFunction(eval, ops, 1);
        opsArray = cat.empty() ? ArrayOf::empty() : cat[0];
    }
    const char* forwarder = subtypes.front() == "()" ? "__nls_ridx_paren_ref__"
        : subtypes.front() == "{}"                   ? "__nls_ridx_brace_ref__"
                                                     : "__nls_ridx_dot_ref__";
    std::string resolved;
    if (!classdefResolveMethodFunction(
            base.classType(), forwarder, resolved, eval->getClassdefAccessContext())) {
        return false;
    }
    FunctionDef* hook = nullptr;
    if (!ctx->lookupFunction(resolved, hook) || hook == nullptr) {
        return false;
    }
    ArrayOfVector hookArgs;
    hookArgs.push_back(base);
    hookArgs.push_back(opsArray);
    ArrayOfVector hookResult = hook->evaluateFunction(eval, hookArgs, 1);
    result = hookResult.empty() ? ArrayOf::empty() : hookResult[0];
    return true;
}
//=============================================================================
// x.(v) on an object with v numeric or logical (T.(3)): the overloaded
// subsref/subsasgn receives v as it is.
bool
tryExtractClassDynamicField(Evaluator* eval, const ArrayOf& base, const ArrayOf& fieldValue,
    int nLhs, ArrayOfVector& result)
{
    if (!base.isClassType() || !(fieldValue.isNumeric() || fieldValue.isLogical())) {
        return false;
    }
    stringVector subtypes = { "." };
    ArrayOfVector subsindices;
    subsindices.push_back(fieldValue);
    bool haveFunction = false;
    result = eval->bytecodeExtractClass(base, subtypes, subsindices, haveFunction, nLhs);
    if (!haveFunction) {
        Error(
            L"Nelson:interpreter:invalidDynamicFieldName", L"Dynamic field name must be a string.");
    }
    return true;
}
//=============================================================================
bool
tryAssignClassDynamicField(
    Evaluator* eval, ArrayOf& base, const ArrayOf& fieldValue, const ArrayOf& value)
{
    if (!base.isClassType() || !(fieldValue.isNumeric() || fieldValue.isLogical())) {
        return false;
    }
    stringVector subtypes = { "." };
    ArrayOfVector subsindices;
    subsindices.push_back(fieldValue);
    bool haveFunction = false;
    ArrayOf assigned = eval->bytecodeAssignClass(base, subtypes, subsindices, value, haveFunction);
    if (!haveFunction) {
        Error(
            L"Nelson:interpreter:invalidDynamicFieldName", L"Dynamic field name must be a string.");
    }
    base = assigned;
    return true;
}
//=============================================================================
bool
tryFastContainersMapLookup(const ArrayOf& base, const ArrayOfVector& indices, ArrayOf& result)
{
    if (!base.isHandle() || base.handleCategory() != NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR
        || indices.size() != 1 || !indices[0].isRowVectorCharacterArray()) {
        return false;
    }
    auto* handleObject = dynamic_cast<ClassdefHandleObject*>(base.asHandleScalar());
    if (handleObject == nullptr) {
        return false;
    }
    // char-key maps only: numeric-key maps also hold native storage but must
    // reject a char key through their own method rather than report "no key"
    if (handleObject->getProperty("KeyTypeValue").asUtf8String() != "char") {
        return false;
    }
    const std::string key = indices[0].asUtf8String();
    ArrayOf nativeIndexMap = handleObject->getProperty("NativeIndexMap");
    if (nativeIndexMap.isHandle()) {
        auto* nativeMap = dynamic_cast<ContainersMapObject*>(nativeIndexMap.asHandleScalar());
        if (nativeMap != nullptr) {
            if (nativeMap->lookup(key, result)) {
                return true;
            }
            Error(L"Nelson:Containers:Map:NoKey",
                L"The specified key is not present in this container.");
        }
    }
    ArrayOf indexMap = handleObject->getProperty("IndexMap");
    const std::string field = containersMapCharField(key);
    if (!indexMap.isStruct() || !indexMap.isField(field)) {
        return false;
    }
    result = indexMap.field(field);
    return true;
}
//=============================================================================
static std::string
containersMapCharField(const std::string& key)
{
    std::ostringstream stream;
    stream << 'c' << std::uppercase << std::hex << std::setfill('0');
    for (unsigned char ch : key) {
        stream << '_' << std::setw(4) << static_cast<unsigned int>(ch);
    }
    return stream.str();
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
