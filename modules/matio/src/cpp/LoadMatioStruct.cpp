//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <vector>
#include "LoadMatioStruct.hpp"
#include "LoadMatioVariable.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefSerialization.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "matioHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD "__nelson_classdef_empty_dimensions__"
#define CLASSDEF_DYNAMICPROPS_STR "dynamicprops"
//=============================================================================
static bool
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
static ClassdefHandleObject*
createClassdefHandleObject(Evaluator* eval, const std::string& className, ArrayOf& handleValue)
{
    handleValue = ArrayOf::empty();
    try {
        FunctionDef* constructor = nullptr;
        if (eval != nullptr && eval->getContext() != nullptr
            && eval->getContext()->lookupFunction(className, constructor)
            && constructor != nullptr) {
            ArrayOfVector constructed = constructor->evaluateFunction(eval, {}, 1);
            if (!constructed.empty() && constructed[0].isHandle()) {
                auto* object = dynamic_cast<ClassdefHandleObject*>(constructed[0].asHandleScalar());
                if (object != nullptr && object->category() == className) {
                    handleValue = constructed[0];
                    return object;
                }
            }
        }
    } catch (const NelsonException&) {
    }
    auto* object = new ClassdefHandleObject(className);
    handleValue = ArrayOf::handle(object);
    return object;
}
//=============================================================================
static bool
loadEmptyClassdefStruct(const Dimensions& dims, const stringVector& fieldnames,
    const std::vector<ArrayOf>& loadedValues, ArrayOf& VariableValue)
{
    if (dims.elementCount() != 1) {
        return false;
    }
    indexType classNameIndex = 0;
    indexType isHandleIndex = 0;
    indexType emptyDimsIndex = 0;
    if (!findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_NAME_FIELD, classNameIndex)
        || !findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_HANDLE_FIELD, isHandleIndex)
        || !findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_EMPTY_DIMS_FIELD, emptyDimsIndex)) {
        return false;
    }
    std::string className;
    bool isHandleObject = false;
    Dimensions objectDims;
    try {
        className = loadedValues[classNameIndex].asUtf8String();
        isHandleObject = loadedValues[isHandleIndex].asLogicalScalar() != 0;
        std::vector<double> dimensionValues = loadedValues[emptyDimsIndex].asDoubleVector();
        for (indexType k = 0; k < static_cast<indexType>(dimensionValues.size()); ++k) {
            objectDims[k] = static_cast<indexType>(dimensionValues[k]);
        }
    } catch (const NelsonException&) {
        return false;
    }
    if (className.empty() || objectDims.elementCount() != 0 || !classdefLoadClass(className)) {
        return false;
    }
    if (isHandleObject && classdefIsHandleClass(className)) {
        VariableValue = ArrayOf::emptyHandle(className, objectDims);
        return true;
    }
    stringVector objectFieldnames;
    for (const auto& property : classdefPropertyDefinitions(className)) {
        if (!classdefHasDependentProperty(className, property.name)
            && !classdefPropertyHasAttribute(property, "Transient")) {
            objectFieldnames.push_back(property.name);
        }
    }
    VariableValue = ArrayOf::emptyClassdef(className, objectFieldnames, objectDims);
    VariableValue.setClassdefLayout(classdefLayout(className));
    return true;
}
//=============================================================================
static bool
loadClassdefStruct(Evaluator* eval, const Dimensions& dims, const stringVector& fieldnames,
    const std::vector<ArrayOf>& loadedValues, ArrayOf& VariableValue)
{
    if (loadEmptyClassdefStruct(dims, fieldnames, loadedValues, VariableValue)) {
        return true;
    }
    indexType classNameIndex = 0;
    indexType isHandleIndex = 0;
    if (!findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_NAME_FIELD, classNameIndex)
        || !findFieldIndex(fieldnames, CLASSDEF_SERIALIZATION_HANDLE_FIELD, isHandleIndex)
        || dims.elementCount() == 0) {
        return false;
    }

    std::string className;
    bool isHandleObject = false;
    try {
        className = loadedValues[classNameIndex].asUtf8String();
        isHandleObject = loadedValues[isHandleIndex].asLogicalScalar() != 0;
    } catch (const NelsonException&) {
        return false;
    }

    if (!classdefLoadClass(className)) {
        return false;
    }

    stringVector objectFieldnames;
    std::vector<indexType> objectFieldIndexes;
    for (indexType k = 0; k < static_cast<indexType>(fieldnames.size()); ++k) {
        if (fieldnames[k] == CLASSDEF_SERIALIZATION_NAME_FIELD
            || fieldnames[k] == CLASSDEF_SERIALIZATION_HANDLE_FIELD) {
            continue;
        }
        objectFieldnames.push_back(fieldnames[k]);
        objectFieldIndexes.push_back(k);
    }

    indexType elementCount = dims.elementCount();
    indexType sourceFieldCount = static_cast<indexType>(fieldnames.size());
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[elementCount * objectFieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }
    for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
        for (indexType fieldIndex = 0; fieldIndex < static_cast<indexType>(objectFieldnames.size());
            ++fieldIndex) {
            indexType sourceOffset
                = elementIndex * sourceFieldCount + objectFieldIndexes[fieldIndex];
            indexType destinationOffset
                = elementIndex * static_cast<indexType>(objectFieldnames.size()) + fieldIndex;
            elements[destinationOffset] = loadedValues[sourceOffset];
        }
    }
    ArrayOf savedObject(NLS_CLASSDEF_ARRAY, dims, elements, false, objectFieldnames);
    savedObject.setClassType(className);
    if (classdefRestoreLoadObject(eval, className, savedObject, VariableValue)) {
        return true;
    }

    if (isHandleObject && classdefIsHandleClass(className)) {
        HandleRef* handles = static_cast<HandleRef*>(
            ArrayOf::allocateArrayOf(NLS_HANDLE, elementCount, stringVector(), false));
        for (indexType elementIndex = 0; elementIndex < elementCount; ++elementIndex) {
            ArrayOf handleValue;
            auto* object = createClassdefHandleObject(eval, className, handleValue);
            for (indexType fieldIndex = 0;
                fieldIndex < static_cast<indexType>(objectFieldnames.size()); ++fieldIndex) {
                const std::string& fieldname = objectFieldnames[fieldIndex];
                if (!classdefHasProperty(className, fieldname)) {
                    if (!classdefIsA(className, CLASSDEF_DYNAMICPROPS_STR)) {
                        continue;
                    }
                    object->addDynamicProperty(fieldname);
                }
                indexType sourceOffset
                    = elementIndex * sourceFieldCount + objectFieldIndexes[fieldIndex];
                object->setProperty(fieldname, loadedValues[sourceOffset]);
            }
            const auto* handleData = static_cast<const HandleRef*>(handleValue.dataPointer());
            handles[elementIndex] = handleData == nullptr
                ? HandleManager::getInstance()->addHandleRef(object)
                : handleData[0];
        }
        VariableValue = ArrayOf(NLS_HANDLE, dims, handles);
        return true;
    }

    if (classdefRestoreDefaultValues(eval, className, savedObject, VariableValue)) {
        return true;
    }

    savedObject.setClassdefLayout(classdefLayout(className));
    VariableValue = savedObject;
    return true;
}
//=============================================================================
bool
LoadMatioStruct(Evaluator* eval, matvar_t* matVariable, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    if (matVariable == nullptr) {
        return bSuccess;
    }
    Dimensions dims = getMatVarDimensions(matVariable);
    unsigned int nbFields = Mat_VarGetNumberOfFields(matVariable);
    char* const* names = Mat_VarGetStructFieldnames(matVariable);
    stringVector fieldnames;
    fieldnames.reserve((size_t)nbFields);
    for (unsigned int k = 0; k < nbFields; k++) {
        fieldnames.push_back(names[k]);
    }

    if (dims.isEmpty(false)) {
        VariableValue = ArrayOf::emptyStruct(fieldnames, dims);
        bSuccess = true;
    } else if (dims.equals(Dimensions(1, 1)) && fieldnames.empty()) {
        VariableValue = ArrayOf::emptyStruct();
        bSuccess = true;
    } else {
        indexType nbElements = dims.elementCount();
        std::vector<ArrayOf> loadedValues(nbElements * fieldnames.size());
        indexType offset = 0;
        for (indexType j = 0; j < nbElements; j++) {
            for (const auto& name : fieldnames) {
                matvar_t* fieldMatVar = Mat_VarGetStructFieldByName(matVariable, name.c_str(), j);
                bool bSuccess = LoadMatioVariable(eval, fieldMatVar, true, loadedValues[offset]);
                if (!bSuccess) {
                    return false;
                }
                offset++;
            }
        }
        if (loadClassdefStruct(eval, dims, fieldnames, loadedValues, VariableValue)) {
            return true;
        }

        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements * fieldnames.size()];
        } catch (const std::bad_alloc&) {
            return false;
        }
        for (indexType k = 0; k < nbElements * static_cast<indexType>(fieldnames.size()); ++k) {
            elements[k] = loadedValues[k];
        }
        VariableValue = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
