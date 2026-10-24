//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadStruct.hpp"
#include "ClassdefHandleObject.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefSerialization.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "HandleManager.hpp"
#include "h5LoadString.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define CLASSDEF_DYNAMICPROPS_STR "dynamicprops"
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
bool
h5LoadStruct(Evaluator* eval, hid_t fid, const std::string& location,
    const std::string& variableName, bool isEmpty, const Dimensions& dims, bool isObject,
    const std::string& objectClassname, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    Dimensions dimsFieldnames = getNelsonDimensions(fid, h5path, FIELDNAMES_STR);
    bool isEmptyFieldnames = isNelsonEmpty(fid, h5path, FIELDNAMES_STR);
    stringVector fieldnames;
    ArrayOf fieldnamesArrayOf;
    if (h5LoadStringArray(eval, fid, h5path, FIELDNAMES_STR, isEmptyFieldnames, dimsFieldnames,
            fieldnamesArrayOf)) {
        fieldnames = fieldnamesArrayOf.asUtf8StringVector(false);
    } else {
        return false;
    }
    indexType nbElements = dims.elementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[nbElements * fieldnames.size()];
    } catch (const std::bad_alloc&) {
        return false;
    }
    bool isClassdefObject = isObject && classdefLoadClass(objectClassname);
    NelsonType objectType
        = isObject ? (isClassdefObject ? NLS_CLASSDEF_ARRAY : NLS_CLASS_ARRAY) : NLS_STRUCT_ARRAY;
    VariableValue = ArrayOf(objectType, dims, elements, false, fieldnames);
    indexType length = dims.elementCount();
    indexType offset = 0;
    for (indexType j = 0; j < length; j++) {
        for (indexType i = 0; i < (sizeType)fieldnames.size(); i++) {
            std::string name = std::to_string(offset);
            bool bSuccess
                = h5LoadVariable(eval, fid, h5path + std::string("/"), name, elements[offset]);
            if (!bSuccess) {
                return false;
            }
            offset++;
        }
    }
    bSuccess = true;
    if (isObject) {
        VariableValue.setClassType(objectClassname);
        if (isClassdefObject) {
            ArrayOf restoredObject;
            if (classdefRestoreLoadObject(eval, objectClassname, VariableValue, restoredObject)) {
                VariableValue = restoredObject;
                return true;
            }
        }
        if (isClassdefObject && classdefIsHandleClass(objectClassname)) {
            if (length == 0) {
                Dimensions handleDims(dims);
                VariableValue = ArrayOf::emptyHandle(objectClassname, handleDims);
                return true;
            }
            HandleRef* handles = static_cast<HandleRef*>(
                ArrayOf::allocateArrayOf(NLS_HANDLE, length, stringVector(), false));
            for (indexType elementIndex = 0; elementIndex < length; ++elementIndex) {
                ArrayOf handleValue;
                auto* object = createClassdefHandleObject(eval, objectClassname, handleValue);
                for (indexType fieldIndex = 0; fieldIndex < (indexType)fieldnames.size();
                    ++fieldIndex) {
                    const std::string& fieldname = fieldnames[fieldIndex];
                    if (!classdefHasProperty(objectClassname, fieldname)) {
                        if (!classdefIsA(objectClassname, CLASSDEF_DYNAMICPROPS_STR)) {
                            continue;
                        }
                        object->addDynamicProperty(fieldname);
                    }
                    indexType propertyOffset
                        = elementIndex * static_cast<indexType>(fieldnames.size()) + fieldIndex;
                    object->setProperty(fieldname, elements[propertyOffset]);
                }
                const auto* handleData = static_cast<const HandleRef*>(handleValue.dataPointer());
                handles[elementIndex] = handleData == nullptr
                    ? HandleManager::getInstance()->addHandleRef(object)
                    : handleData[0];
            }
            VariableValue = ArrayOf(NLS_HANDLE, dims, handles);
        } else if (isClassdefObject) {
            ArrayOf restoredObject;
            if (classdefRestoreDefaultValues(
                    eval, objectClassname, VariableValue, restoredObject)) {
                VariableValue = restoredObject;
            }
        }
        if (isClassdefObject && VariableValue.isClassdefType()) {
            VariableValue.setClassdefLayout(classdefLayout(objectClassname));
        }
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
