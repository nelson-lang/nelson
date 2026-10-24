//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <vector>
#include "ClassdefSerialization.hpp"
#include "ClassdefParser.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    constexpr const char* CLASSDEF_SAVE_OBJECT_IMPL = "saveObjectImpl";
    constexpr const char* CLASSDEF_LOAD_OBJECT_IMPL = "loadObjectImpl";
    constexpr const char* CLASSDEF_ATTR_TRANSIENT = "Transient";
    //=============================================================================
    ArrayOf
    asStructPayload(const ArrayOf& value);
    //=============================================================================
    std::string
    classdefObjectClassName(const ArrayOf& objectValue)
    {
        if (objectValue.isClassdefType()) {
            return objectValue.classType();
        }
        if (objectValue.isHandle()) {
            const std::string className = objectValue.handleClassName();
            if (classdefLoadClass(className)) {
                return className;
            }
        }
        return {};
    }
    //=============================================================================
    bool
    findFieldIndex(const stringVector& fieldnames, const std::string& fieldname, indexType& index)
    {
        for (indexType k = 0; k < static_cast<indexType>(fieldnames.size()); ++k) {
            if (fieldnames[k] == fieldname) {
                index = k;
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    isTransientProperty(const std::string& className, const std::string& fieldname)
    {
        ClassdefPropertyDefinition property;
        return classdefPropertyDefinition(className, fieldname, property)
            && classdefPropertyHasAttribute(property, CLASSDEF_ATTR_TRANSIENT);
    }
    //=============================================================================
    stringVector
    persistentFieldnames(const std::string& className, const stringVector& fieldnames)
    {
        stringVector result;
        result.reserve(fieldnames.size());
        for (const auto& fieldname : fieldnames) {
            if (!isTransientProperty(className, fieldname)) {
                result.push_back(fieldname);
            }
        }
        return result;
    }
    //=============================================================================
    ArrayOf
    copyClassdefFields(
        const ArrayOf& value, const stringVector& tarfieldnames, NelsonType targetType)
    {
        const stringVector sourceFieldnames = value.fieldNames();
        const Dimensions dims = value.dimensions();
        const indexType elementCount = dims.elementCount();
        const indexType sourceFieldCount = static_cast<indexType>(sourceFieldnames.size());
        const indexType tarfieldCount = static_cast<indexType>(tarfieldnames.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(targetType, elementCount, tarfieldnames, false));
        const auto* source = static_cast<const ArrayOf*>(value.dataPointer());
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            for (indexType fieldIndex = 0; fieldIndex < tarfieldCount; ++fieldIndex) {
                indexType sourceIndex = 0;
                if (findFieldIndex(sourceFieldnames, tarfieldnames[fieldIndex], sourceIndex)) {
                    elements[elementIndex * tarfieldCount + fieldIndex]
                        = source[elementIndex * sourceFieldCount + sourceIndex];
                }
            }
        }
        ArrayOf result(targetType, dims, elements, false, tarfieldnames);
        if (targetType == NLS_CLASSDEF_ARRAY) {
            result.setClassType(value.classType());
        }
        return result;
    }
    //=============================================================================
    ArrayOf
    scalarElementAsStruct(const ArrayOf& value, indexType elementIndex)
    {
        const stringVector fieldnames = value.fieldNames();
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, 1, fieldnames, false));
        for (indexType fieldIndex = 0; fieldIndex < static_cast<indexType>(fieldnames.size());
            ++fieldIndex) {
            ArrayOfVector values = value.fieldAsList(fieldnames[fieldIndex]);
            if (elementIndex < static_cast<indexType>(values.size())) {
                elements[fieldIndex] = values[elementIndex];
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, Dimensions(1, 1), elements, false, fieldnames);
    }
    //=============================================================================
    ArrayOf
    scalarElementAsClassdef(const ArrayOf& value, indexType elementIndex)
    {
        const stringVector fieldnames = value.fieldNames();
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, 1, fieldnames, false));
        for (indexType fieldIndex = 0; fieldIndex < static_cast<indexType>(fieldnames.size());
            ++fieldIndex) {
            ArrayOfVector values = value.fieldAsList(fieldnames[fieldIndex]);
            if (elementIndex < static_cast<indexType>(values.size())) {
                elements[fieldIndex] = values[elementIndex];
            }
        }
        const std::string className = value.classType();
        ArrayOf result(NLS_CLASSDEF_ARRAY, Dimensions(1, 1), elements, false, fieldnames);
        result.setClassType(className);
        result.setClassdefLayout(classdefLayout(className));
        return result;
    }
    //=============================================================================
    ArrayOf
    scalarElementAsHandle(const ArrayOf& value, indexType elementIndex)
    {
        const auto* handles = static_cast<const HandleRef*>(value.dataPointer());
        if (handles == nullptr) {
            return ArrayOf::empty();
        }
        void* data = ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false);
        auto* ref = static_cast<HandleRef*>(data);
        ref[0] = handles[elementIndex];
        return ArrayOf(NLS_HANDLE, Dimensions(1, 1), data);
    }
    //=============================================================================
    ArrayOf
    scalarObjectElement(const ArrayOf& value, indexType elementIndex)
    {
        if (value.isClassdefType()) {
            return scalarElementAsClassdef(value, elementIndex);
        }
        if (value.isHandle()) {
            return scalarElementAsHandle(value, elementIndex);
        }
        return value;
    }
    //=============================================================================
    bool
    handlesAreValid(const ArrayOf& value)
    {
        if (!value.isHandle()) {
            return true;
        }
        const auto* handles = static_cast<const HandleRef*>(value.dataPointer());
        const indexType elementCount = value.elementCount();
        for (indexType k = 0; handles != nullptr && k < elementCount; ++k) {
            if (!handles[k] || !handles[k]->isValid() || handles[k]->nativePointer() == nullptr) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    fieldnamesMatch(const stringVector& left, const stringVector& right)
    {
        return left.size() == right.size()
            && std::equal(left.begin(), left.end(), right.begin(), right.end());
    }
    //=============================================================================
    ArrayOf
    classdefArrayFromScalarOutputs(
        const std::string& className, const Dimensions& dims, const std::vector<ArrayOf>& outputs)
    {
        if (outputs.empty()) {
            Error("Nelson:interpreter:invalidLoadObject", "loadObjectImpl must return a value.");
        }
        if (outputs.front().isHandle()) {
            const indexType elementCount = dims.elementCount();
            auto* handles = static_cast<HandleRef*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, elementCount, stringVector(), false));
            for (indexType k = 0; k < elementCount; ++k) {
                if (!outputs[k].isHandle() || outputs[k].handleClassName() != className) {
                    Error("Nelson:interpreter:invalidLoadObject",
                        "loadObjectImpl must return a value.");
                }
                const auto* handleData = static_cast<const HandleRef*>(outputs[k].dataPointer());
                handles[k] = handleData == nullptr ? HandleRef() : handleData[0];
            }
            return ArrayOf(NLS_HANDLE, dims, handles);
        }

        if (!outputs.front().isClassdefType() || outputs.front().classType() != className) {
            Error("Nelson:interpreter:invalidLoadObject", "loadObjectImpl must return a value.");
        }
        const stringVector fieldnames = outputs.front().fieldNames();
        const indexType elementCount = dims.elementCount();
        const indexType fieldCount = static_cast<indexType>(fieldnames.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, elementCount, fieldnames, false));
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            if (!outputs[elementIndex].isClassdefType()
                || outputs[elementIndex].classType() != className
                || !outputs[elementIndex].isScalar()
                || !fieldnamesMatch(fieldnames, outputs[elementIndex].fieldNames())) {
                Error(
                    "Nelson:interpreter:invalidLoadObject", "loadObjectImpl must return a value.");
            }
            const auto* source = static_cast<const ArrayOf*>(outputs[elementIndex].dataPointer());
            for (indexType fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex) {
                elements[elementIndex * fieldCount + fieldIndex] = source[fieldIndex];
            }
        }
        ArrayOf result(NLS_CLASSDEF_ARRAY, dims, elements, false, fieldnames);
        result.setClassType(className);
        result.setClassdefLayout(classdefLayout(className));
        return result;
    }
    //=============================================================================
    ArrayOf
    classdefPayloadFromScalarHookOutputs(
        const std::string& className, const Dimensions& dims, const std::vector<ArrayOf>& outputs)
    {
        if (outputs.empty()) {
            Error("Nelson:interpreter:saveObjectNoReturnValue",
                "saveObjectImpl must return a value.");
        }
        ArrayOf firstPayload = asStructPayload(outputs.front());
        if (!firstPayload.isStruct() || !firstPayload.isScalar()) {
            Error("Nelson:interpreter:saveObjectNotStruct", "saveObjectImpl must return a struct.");
        }
        const stringVector fieldnames = firstPayload.fieldNames();
        const indexType elementCount = dims.elementCount();
        const indexType fieldCount = static_cast<indexType>(fieldnames.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, elementCount, fieldnames, false));
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            ArrayOf payload = asStructPayload(outputs[elementIndex]);
            if (!payload.isStruct() || !payload.isScalar()
                || !fieldnamesMatch(fieldnames, payload.fieldNames())) {
                Error("Nelson:interpreter:saveObjectNotStruct",
                    "saveObjectImpl must return a struct.");
            }
            const auto* source = static_cast<const ArrayOf*>(payload.dataPointer());
            for (indexType fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex) {
                elements[elementIndex * fieldCount + fieldIndex] = source[fieldIndex];
            }
        }
        ArrayOf result(NLS_CLASSDEF_ARRAY, dims, elements, false, fieldnames);
        result.setClassType(className);
        return result;
    }
    //=============================================================================
    bool
    evaluateDefaultClassdefValue(Evaluator* eval, const std::string& className, ArrayOf& value)
    {
        if (eval == nullptr || eval->getContext() == nullptr) {
            return false;
        }
        FunctionDef* constructor = nullptr;
        if (!eval->getContext()->lookupFunction(className, constructor) || constructor == nullptr) {
            return false;
        }
        try {
            ArrayOfVector result = constructor->evaluateFunction(eval, {}, 1);
            if (result.empty() || !result[0].isClassdefType() || result[0].classType() != className
                || !result[0].isScalar()) {
                return false;
            }
            value = result[0];
            return true;
        } catch (const NelsonException&) {
            return false;
        }
    }
    //=============================================================================
    bool
    resolveSaveHook(Evaluator* eval, const std::string& className, FunctionDef*& functionDef)
    {
        std::string functionName;
        functionDef = nullptr;
        return classdefResolveMethodFunction(
                   className, CLASSDEF_SAVE_OBJECT_IMPL, functionName, className)
            && eval != nullptr && eval->getContext()->lookupFunction(functionName, functionDef)
            && functionDef != nullptr;
    }
    //=============================================================================
    bool
    resolveLoadHook(Evaluator* eval, const std::string& className, FunctionDef*& functionDef)
    {
        std::string functionName;
        functionDef = nullptr;
        return classdefResolveStaticMethodFunction(
                   className, CLASSDEF_LOAD_OBJECT_IMPL, functionName, className)
            && eval != nullptr && eval->getContext()->lookupFunction(functionName, functionDef)
            && functionDef != nullptr;
    }
    //=============================================================================
    ArrayOf
    asStructPayload(const ArrayOf& value)
    {
        if (value.isStruct()) {
            return value;
        }
        if (!value.isClassdefType()) {
            return value;
        }
        stringVector fieldnames = value.fieldNames();
        Dimensions dims = value.dimensions();
        indexType elementCount = dims.elementCount();
        indexType fieldCount = static_cast<indexType>(fieldnames.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, elementCount, fieldnames, false));
        const auto* source = static_cast<const ArrayOf*>(value.dataPointer());
        for (indexType k = 0; k < elementCount * fieldCount; ++k) {
            elements[k] = source[k];
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    }
    //=============================================================================
    ArrayOf
    asClassdefPayload(const std::string& className, const ArrayOf& value)
    {
        ArrayOf payload = asStructPayload(value);
        if (!payload.isStruct()) {
            Error("Nelson:interpreter:saveObjectNotStruct", "saveObjectImpl must return a struct.");
        }
        stringVector fieldnames = payload.fieldNames();
        Dimensions dims = payload.dimensions();
        indexType elementCount = dims.elementCount();
        indexType fieldCount = static_cast<indexType>(fieldnames.size());
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, elementCount, fieldnames, false));
        const auto* source = static_cast<const ArrayOf*>(payload.dataPointer());
        for (indexType k = 0; k < elementCount * fieldCount; ++k) {
            elements[k] = source[k];
        }
        ArrayOf result(NLS_CLASSDEF_ARRAY, dims, elements, false, fieldnames);
        result.setClassType(className);
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
classdefPrepareSaveObject(Evaluator* eval, const ArrayOf& objectValue, ArrayOf& savedValue)
{
    const std::string className = classdefObjectClassName(objectValue);
    if (className.empty()) {
        return false;
    }

    FunctionDef* saveFunction = nullptr;
    FunctionDef* loadFunction = nullptr;
    if (!resolveSaveHook(eval, className, saveFunction)
        || !resolveLoadHook(eval, className, loadFunction)) {
        return false;
    }

    const Dimensions dims = objectValue.dimensions();
    const indexType elementCount = dims.elementCount();
    if (!handlesAreValid(objectValue)) {
        Error("Nelson:interpreter:invalidHandle", "Invalid handle.");
    }
    if (elementCount == 0) {
        if (objectValue.isHandle()) {
            Dimensions emptyDims = dims;
            savedValue = ArrayOf::emptyHandle(className, emptyDims);
        } else {
            savedValue = asClassdefPayload(className, objectValue);
        }
        return true;
    }
    if (elementCount > 1) {
        std::vector<ArrayOf> outputs;
        outputs.reserve(elementCount);
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            ArrayOfVector input;
            input.push_back(scalarObjectElement(objectValue, elementIndex));
            ArrayOfVector output = saveFunction->evaluateFunction(eval, input, 1);
            if (output.empty()) {
                Error("Nelson:interpreter:saveObjectNoReturnValue",
                    "saveObjectImpl must return a value.");
            }
            outputs.push_back(output[0]);
        }
        savedValue = classdefPayloadFromScalarHookOutputs(className, dims, outputs);
        return true;
    }

    ArrayOfVector input;
    input.push_back(objectValue);
    ArrayOfVector output = saveFunction->evaluateFunction(eval, input, 1);
    if (output.empty()) {
        Error("Nelson:interpreter:saveObjectNoReturnValue", "saveObjectImpl must return a value.");
    }
    savedValue = asClassdefPayload(className, output[0]);
    return true;
}
//=============================================================================
bool
classdefPreparePersistentObject(const ArrayOf& objectValue, ArrayOf& savedValue)
{
    if (!objectValue.isClassdefType()) {
        return false;
    }
    const std::string className = objectValue.classType();
    if (className.empty() || !classdefLoadClass(className)) {
        return false;
    }
    savedValue = copyClassdefFields(
        objectValue, persistentFieldnames(className, objectValue.fieldNames()), NLS_CLASSDEF_ARRAY);
    return true;
}
//=============================================================================
bool
classdefToStruct(const ArrayOf& objectValue, ArrayOf& structValue)
{
    if (!objectValue.isClassdefType()) {
        return false;
    }
    const std::string className = objectValue.classType();
    if (className.empty() || !classdefLoadClass(className)) {
        return false;
    }
    structValue = copyClassdefFields(
        objectValue, persistentFieldnames(className, objectValue.fieldNames()), NLS_STRUCT_ARRAY);
    return true;
}
//=============================================================================
bool
classdefRestoreLoadObject(
    Evaluator* eval, const std::string& className, const ArrayOf& savedValue, ArrayOf& objectValue)
{
    if (!classdefLoadClass(className)) {
        return false;
    }
    FunctionDef* loadFunction = nullptr;
    if (!resolveLoadHook(eval, className, loadFunction)) {
        return false;
    }

    const Dimensions dims = savedValue.dimensions();
    const indexType elementCount = dims.elementCount();
    if (elementCount == 0) {
        Dimensions emptyDims = dims;
        if (classdefIsHandleClass(className)) {
            objectValue = ArrayOf::emptyHandle(className, emptyDims);
        } else {
            objectValue = ArrayOf::emptyClassdef(className, savedValue.fieldNames(), emptyDims);
            objectValue.setClassdefLayout(classdefLayout(className));
        }
        return true;
    }
    if (elementCount > 1) {
        std::vector<ArrayOf> outputs;
        outputs.reserve(elementCount);
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            ArrayOfVector input;
            input.push_back(scalarElementAsStruct(savedValue, elementIndex));
            ArrayOfVector output = loadFunction->evaluateFunction(eval, input, 1);
            if (output.empty()) {
                Error(
                    "Nelson:interpreter:invalidLoadObject", "loadObjectImpl must return a value.");
            }
            outputs.push_back(output[0]);
        }
        objectValue = classdefArrayFromScalarOutputs(className, dims, outputs);
        return true;
    }

    ArrayOfVector input;
    input.push_back(asStructPayload(savedValue));
    ArrayOfVector output = loadFunction->evaluateFunction(eval, input, 1);
    if (output.empty()) {
        Error("Nelson:interpreter:invalidLoadObject", "loadObjectImpl must return a value.");
    }
    objectValue = output[0];
    return true;
}
//=============================================================================
bool
classdefRestoreDefaultValues(
    Evaluator* eval, const std::string& className, const ArrayOf& savedValue, ArrayOf& objectValue)
{
    if (!savedValue.isClassdefType() || className.empty() || !classdefLoadClass(className)
        || classdefIsHandleClass(className)) {
        return false;
    }
    ClassdefLayoutPtr layout = classdefLayout(className);
    if (layout == nullptr || layout->storagePropertyNames.empty()) {
        return false;
    }
    const stringVector savedFieldnames = savedValue.fieldNames();
    bool hasMissingField = false;
    for (const auto& fieldname : layout->storagePropertyNames) {
        indexType index = 0;
        if (!findFieldIndex(savedFieldnames, fieldname, index)) {
            hasMissingField = true;
            break;
        }
    }
    if (!hasMissingField) {
        return false;
    }

    const Dimensions dims = savedValue.dimensions();
    const indexType elementCount = dims.elementCount();
    const indexType fullFieldCount = static_cast<indexType>(layout->storagePropertyNames.size());
    const indexType savedFieldCount = static_cast<indexType>(savedFieldnames.size());
    const auto* savedElements = static_cast<const ArrayOf*>(savedValue.dataPointer());
    auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
        NLS_CLASSDEF_ARRAY, elementCount, layout->storagePropertyNames, false));
    for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
        ArrayOf defaults;
        if (!evaluateDefaultClassdefValue(eval, className, defaults)) {
            delete[] elements;
            return false;
        }
        const stringVector defaultFieldnames = defaults.fieldNames();
        const auto* defaultElements = static_cast<const ArrayOf*>(defaults.dataPointer());
        for (indexType fieldIndex = 0; fieldIndex < fullFieldCount; ++fieldIndex) {
            indexType defaultIndex = 0;
            if (findFieldIndex(
                    defaultFieldnames, layout->storagePropertyNames[fieldIndex], defaultIndex)) {
                elements[elementIndex * fullFieldCount + fieldIndex]
                    = defaultElements[defaultIndex];
            }
        }
        for (indexType savedFieldIndex = 0; savedFieldIndex < savedFieldCount; ++savedFieldIndex) {
            indexType targetIndex = 0;
            if (findFieldIndex(
                    layout->storagePropertyNames, savedFieldnames[savedFieldIndex], targetIndex)) {
                elements[elementIndex * fullFieldCount + targetIndex]
                    = savedElements[elementIndex * savedFieldCount + savedFieldIndex];
            }
        }
    }
    objectValue = ArrayOf(NLS_CLASSDEF_ARRAY, dims, elements, false, layout->storagePropertyNames);
    objectValue.setClassType(className);
    objectValue.setClassdefLayout(layout);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
