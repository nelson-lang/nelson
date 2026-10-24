//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "containersMapBuiltin.hpp"
#include "ClassName.hpp"
#include "ClassdefHandleObject.hpp"
#include "ContainersMapObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <memory>
#include <unordered_map>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ClassdefHandleObject*
getMapObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()
        || value.handleCategory() != NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR) {
        Error(L"Nelson:containersMap:objectExpected", L"containers.Map object expected.");
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(value.asHandleScalar());
    if (object == nullptr) {
        Error(L"Nelson:containersMap:objectExpected", L"containers.Map object expected.");
    }
    return object;
}
//=============================================================================
ContainersMapObject*
getNativeMapObject(ClassdefHandleObject* object)
{
    ArrayOf nativeIndexMap = object->getProperty("NativeIndexMap");
    if (!nativeIndexMap.isHandle()) {
        Error(L"Nelson:containersMap:nativeStorageExpected",
            L"Native containers.Map storage expected.");
    }
    auto* nativeMap = dynamic_cast<ContainersMapObject*>(nativeIndexMap.asHandleScalar());
    if (nativeMap == nullptr) {
        Error(L"Nelson:containersMap:nativeStorageExpected",
            L"Native containers.Map storage expected.");
    }
    return nativeMap;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
bool
isNumericScalarKey(const ArrayOf& value)
{
    return value.isNumeric() && value.isScalar() && !value.isComplex() && !value.isSparse();
}
//=============================================================================
// Byte-exact canonical encoding of a numeric scalar key. The .m layer always
// casts the key to the map key type first, so dataClass() is one of the
// supported numeric types. -0.0 is normalized to +0.0 so 0 and -0 share a key,
// matching the hashing used by the interpreted fallback.
std::string
encodeNumericKey(const ArrayOf& key)
{
    std::string encoded(1, static_cast<char>(key.dataClass()));
    switch (key.dataClass()) {
    case NLS_DOUBLE: {
        double v = key.asDoubleScalar();
        if (v == static_cast<double>(0)) {
            v = static_cast<double>(0);
        }
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    case NLS_SINGLE: {
        single v = static_cast<single>(key.asSingleScalar());
        if (v == static_cast<single>(0)) {
            v = static_cast<single>(0);
        }
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    case NLS_INT32: {
        int32 v = key.asInt32Scalar();
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    case NLS_UINT32: {
        uint32 v = key.asUint32Scalar();
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    case NLS_INT64: {
        int64 v = key.asInt64Scalar();
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    case NLS_UINT64: {
        uint64 v = key.asUint64Scalar();
        encoded.append(reinterpret_cast<const char*>(&v), sizeof(v));
    } break;
    default:
        Error(L"Nelson:containersMap:unsupportedKeyType", L"Unsupported key type.");
    }
    return encoded;
}
//=============================================================================
// Canonical string key for a native action argument: the UTF-8 bytes for a
// char key, the byte-exact encoding for a numeric scalar key.
std::string
canonicalActionKey(const ArrayOf& key)
{
    if (key.isRowVectorCharacterArray()) {
        return key.asUtf8String();
    }
    if (isNumericScalarKey(key)) {
        return encodeNumericKey(key);
    }
    Error(L"Nelson:containersMap:keyTypeMismatch",
        L"Specified key type does not match the type expected for this container.");
    return {};
}
//=============================================================================
std::string
lowerTextScalar(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:validators:mustBeTextScalar",
            L"%s must be a character vector or string scalar.", L"Type name");
    }
    std::string text = value.asUtf8String();
    std::transform(text.begin(), text.end(), text.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return text;
}
//=============================================================================
ArrayOf
makeCell(const ArrayOfVector& values)
{
    Dimensions dims(1, static_cast<indexType>(values.size()));
    if (values.empty()) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    std::unique_ptr<ArrayOf[]> elements = std::make_unique<ArrayOf[]>(values.size());
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = values[k];
    }
    ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
    elements.release();
    return result;
}
//=============================================================================
ArrayOfVector
keysFromInput(const ArrayOf& keys)
{
    ArrayOfVector result;
    if (keys.isCell()) {
        ArrayOfView<ArrayOf> elements(keys, ArrayOfViewAssumeValidated {});
        const indexType elementCount = keys.elementCount();
        result.reserve(static_cast<size_t>(elementCount));
        for (indexType k = 0; k < elementCount; ++k) {
            if (!elements[k].isRowVectorCharacterArray()) {
                Error(L"Nelson:containersMap:keyTypeMismatch",
                    L"Specified key type does not match the type expected for this container.");
            }
            result.push_back(elements[k]);
        }
        return result;
    }
    if (keys.isRowVectorCharacterArray()) {
        result.push_back(keys);
        return result;
    }
    Error(L"Nelson:containersMap:keyTypeMismatch",
        L"Specified key type does not match the type expected for this container.");
    return result;
}
//=============================================================================
ArrayOf
valueAt(const ArrayOf& values, indexType index)
{
    ArrayOf copy = values;
    return copy.valueAtIndex(static_cast<uint64>(index));
}
//=============================================================================
ArrayOfVector
valuesFromInput(const ArrayOf& values, size_t keyCount)
{
    ArrayOfVector result;
    if (values.isCell()) {
        ArrayOfView<ArrayOf> elements(values, ArrayOfViewAssumeValidated {});
        const indexType elementCount = values.elementCount();
        if (elementCount != static_cast<indexType>(keyCount)) {
            Error(L"Nelson:containersMap:keyValueCountMismatch",
                L"The number of keys and values must match.");
        }
        result.reserve(static_cast<size_t>(elementCount));
        for (indexType k = 0; k < elementCount; ++k) {
            result.push_back(elements[k]);
        }
        return result;
    }
    const indexType valueCount = values.elementCount();
    if (values.isDoubleType(true) && !values.isSparse()) {
        ArrayOfView<double> data(values, ArrayOfViewAssumeValidated {});
        if (valueCount == 1 && keyCount > 1) {
            ArrayOf scalar = ArrayOf::doubleScalar(data[0]);
            result.reserve(keyCount);
            for (size_t k = 0; k < keyCount; ++k) {
                result.push_back(scalar);
            }
            return result;
        }
        if (valueCount != static_cast<indexType>(keyCount)) {
            Error(L"Nelson:containersMap:keyValueCountMismatch",
                L"The number of keys and values must match.");
        }
        result.reserve(static_cast<size_t>(valueCount));
        for (indexType k = 0; k < valueCount; ++k) {
            result.push_back(ArrayOf::doubleScalar(data[k]));
        }
        return result;
    }
    if (valueCount == 1 && keyCount > 1) {
        result.reserve(keyCount);
        for (size_t k = 0; k < keyCount; ++k) {
            result.push_back(values);
        }
        return result;
    }
    if (valueCount != static_cast<indexType>(keyCount)) {
        Error(L"Nelson:containersMap:keyValueCountMismatch",
            L"The number of keys and values must match.");
    }
    result.reserve(static_cast<size_t>(valueCount));
    for (indexType k = 0; k < valueCount; ++k) {
        result.push_back(valueAt(values, k));
    }
    return result;
}
//=============================================================================
// a class is the value type only for char values or scalars of one
// numeric or logical class; any other value set is 'any'
std::string
inferValueType(const std::vector<ArrayOf>& values)
{
    if (values.empty()) {
        return "any";
    }
    const ArrayOf& first = values[0];
    if (!(first.isCharacterArray()
            || ((first.isNumeric() || first.isLogical()) && first.isScalar()))) {
        return "any";
    }
    const std::string firstClass = ClassName(first);
    for (size_t k = 1; k < values.size(); ++k) {
        if (ClassName(values[k]) != firstClass
            || (!values[k].isCharacterArray() && !values[k].isScalar())) {
            return "any";
        }
    }
    return firstClass;
}
//=============================================================================
std::string
inferValueType(const ArrayOf& rawValues, const std::vector<ArrayOf>& values)
{
    if (rawValues.isDoubleType(true) && !rawValues.isSparse()) {
        return "double";
    }
    return inferValueType(values);
}
//=============================================================================
bool
canUseDenseDoubleValues(const ArrayOf& values)
{
    return values.isDoubleType(true) && !values.isSparse();
}
//=============================================================================
bool
isSupportedKeyType(const std::string& typeName)
{
    return typeName == "char" || typeName == "double" || typeName == "single" || typeName == "int32"
        || typeName == "uint32" || typeName == "int64" || typeName == "uint64";
}
//=============================================================================
bool
isSupportedValueType(const std::string& typeName)
{
    return typeName == "any" || typeName == "char" || typeName == "double" || typeName == "single"
        || typeName == "int32" || typeName == "uint32" || typeName == "int64"
        || typeName == "uint64" || typeName == "logical" || typeName == "int8"
        || typeName == "uint8";
}
//=============================================================================
ArrayOfVector
nativeAction(ClassdefHandleObject* object, const ArrayOfVector& argIn)
{
    const std::string action = lowerTextScalar(argIn[1]);
    ContainersMapObject* nativeMap = getNativeMapObject(object);
    if (action == "__count") {
        return ArrayOfVector { ArrayOf::doubleScalar(static_cast<double>(nativeMap->count())) };
    }
    if (action == "__keys") {
        return ArrayOfVector { nativeMap->keys() };
    }
    if (action == "__values") {
        return ArrayOfVector { nativeMap->values() };
    }
    if (action == "__lookup" && argIn.size() == 4) {
        ArrayOf value;
        if (!nativeMap->lookup(canonicalActionKey(argIn[2]), value)) {
            Error(L"Nelson:Containers:Map:NoKey",
                L"The specified key is not present in this container.");
        }
        return ArrayOfVector { value };
    }
    if (action == "__iskey" && argIn.size() == 4) {
        return ArrayOfVector { ArrayOf::logicalScalar(
            nativeMap->isKey(canonicalActionKey(argIn[2]))) };
    }
    if (action == "__set" && argIn.size() == 4) {
        if (argIn[2].isRowVectorCharacterArray()) {
            nativeMap->set(argIn[2].asUtf8String(), argIn[3]);
        } else if (isNumericScalarKey(argIn[2])) {
            nativeMap->setWithOriginalKey(encodeNumericKey(argIn[2]), argIn[2], argIn[3]);
        } else {
            Error(L"Nelson:containersMap:keyTypeMismatch",
                L"Specified key type does not match the type expected for this container.");
        }
        return ArrayOfVector {};
    }
    if (action == "__remove" && argIn.size() == 4) {
        return ArrayOfVector { ArrayOf::logicalScalar(
            nativeMap->removeKey(canonicalActionKey(argIn[2]))) };
    }
    Error(L"Nelson:containers:unsupportedAction", L"Unsupported containers.Map native action.");
    return {};
}
//=============================================================================
void
setCharDoubleMap(ClassdefHandleObject* object, const ArrayOf& rawKeys, const ArrayOf& rawValues)
{
    size_t keyCount = 0;
    if (rawKeys.isCell()) {
        keyCount = static_cast<size_t>(rawKeys.elementCount());
    } else if (rawKeys.isRowVectorCharacterArray()) {
        keyCount = 1;
    } else {
        Error(L"Nelson:containersMap:keyTypeMismatch",
            L"Specified key type does not match the type expected for this container.");
    }

    const indexType valueCount = rawValues.elementCount();
    if (valueCount != static_cast<indexType>(keyCount) && !(valueCount == 1 && keyCount > 1)) {
        Error(L"Nelson:containersMap:keyValueCountMismatch",
            L"The number of keys and values must match.");
    }

    ArrayOfView<double> data(rawValues, ArrayOfViewAssumeValidated {});
    std::vector<std::string> uniqueKeys;
    std::vector<double> uniqueValues;
    std::unordered_map<std::string, size_t> positions;
    positions.reserve(keyCount);
    uniqueKeys.reserve(keyCount);
    uniqueValues.reserve(keyCount);

    auto setValue = [&](std::string key, size_t k) {
        const double value = (valueCount == 1) ? data[0] : data[k];
        auto found = positions.find(key);
        if (found == positions.end()) {
            positions.emplace(key, uniqueKeys.size());
            uniqueKeys.push_back(std::move(key));
            uniqueValues.push_back(value);
        } else {
            uniqueValues[found->second] = value;
        }
    };

    if (rawKeys.isCell()) {
        ArrayOfView<ArrayOf> keyElements(rawKeys, ArrayOfViewAssumeValidated {});
        for (size_t k = 0; k < keyCount; ++k) {
            if (!keyElements[k].isRowVectorCharacterArray()) {
                Error(L"Nelson:containersMap:keyTypeMismatch",
                    L"Specified key type does not match the type expected for this container.");
            }
            setValue(keyElements[k].asUtf8String(), k);
        }
    } else {
        for (size_t k = 0; k < keyCount; ++k) {
            setValue(rawKeys.asUtf8String(), k);
        }
    }

    object->setStoredProperty("KeyList", makeCell({}));
    object->setStoredProperty("ValueList", makeCell({}));
    object->setStoredProperty("KeyTypeValue", ArrayOf::characterArray("char"));
    object->setStoredProperty("ValueTypeValue", ArrayOf::characterArray("double"));
    object->setStoredProperty("IndexMap", ArrayOf::structScalar(stringVector {}, ArrayOfVector {}));
    object->setStoredProperty("NativeIndexMap",
        ArrayOf::handle(new ContainersMapObject(
            std::move(uniqueKeys), std::move(uniqueValues), std::move(positions))));
}
//=============================================================================
// Install an empty native storage back end, leaving the key/value type
// properties untouched. Used both for empty typed maps and for numeric-keyed
// construction (keyed inserts then flow through the O(1) native store).
void
setEmptyNativeStorage(ClassdefHandleObject* object)
{
    object->setStoredProperty("KeyList", makeCell({}));
    object->setStoredProperty("ValueList", makeCell({}));
    object->setStoredProperty("IndexMap", ArrayOf::structScalar(stringVector {}, ArrayOfVector {}));
    object->setStoredProperty("NativeIndexMap",
        ArrayOf::handle(
            new ContainersMapObject(std::vector<std::string> {}, std::vector<ArrayOf> {})));
}
//=============================================================================
void
setEmptyTypedMap(
    ClassdefHandleObject* object, const std::string& keyType, const std::string& valueType)
{
    if (!isSupportedKeyType(keyType)) {
        Error(L"Nelson:containersMap:unsupportedKeyType", L"Unsupported key type.");
    }
    if (!isSupportedValueType(valueType)) {
        Error(L"Nelson:containersMap:unsupportedValueType", L"Unsupported value type.");
    }
    object->setStoredProperty("KeyTypeValue", ArrayOf::characterArray(keyType));
    object->setStoredProperty("ValueTypeValue", ArrayOf::characterArray(valueType));
    // Both char and numeric key maps use the native O(1) store; numeric keys
    // carry their original scalar next to a byte-exact string encoding.
    setEmptyNativeStorage(object);
}
//=============================================================================
void
setCharMap(ClassdefHandleObject* object, const ArrayOf& rawKeys, const ArrayOf& rawValues)
{
    // a single key given as text takes the whole value (a cell included)
    const bool singleKey = rawKeys.isRowVectorCharacterArray() || rawKeys.isScalarStringArray();
    if (canUseDenseDoubleValues(rawValues) && !(singleKey && !rawValues.isScalar())) {
        setCharDoubleMap(object, rawKeys, rawValues);
        return;
    }
    ArrayOfVector keys = keysFromInput(rawKeys);
    ArrayOfVector values;
    if (singleKey) {
        if (rawValues.isEmpty()) {
            Error(L"Nelson:containersMap:missingValues",
                L"When keys are specified, values must also be specified.");
        }
        values.push_back(rawValues);
    } else {
        values = valuesFromInput(rawValues, keys.size());
    }
    std::vector<std::string> uniqueKeys;
    std::vector<ArrayOf> uniqueValues;
    std::unordered_map<std::string, size_t> positions;
    positions.reserve(keys.size());
    uniqueKeys.reserve(keys.size());
    uniqueValues.reserve(keys.size());

    for (size_t k = 0; k < keys.size(); ++k) {
        const std::string key = keys[k].asUtf8String();
        auto found = positions.find(key);
        if (found == positions.end()) {
            positions.emplace(key, uniqueKeys.size());
            uniqueKeys.push_back(key);
            uniqueValues.push_back(values[k]);
        } else {
            uniqueValues[found->second] = values[k];
        }
    }

    object->setStoredProperty("KeyList", makeCell({}));
    object->setStoredProperty("ValueList", makeCell({}));
    object->setStoredProperty("KeyTypeValue", ArrayOf::characterArray("char"));
    object->setStoredProperty(
        "ValueTypeValue", ArrayOf::characterArray(inferValueType(rawValues, uniqueValues)));
    object->setStoredProperty("IndexMap", ArrayOf::structScalar(stringVector {}, ArrayOfVector {}));
    object->setStoredProperty("NativeIndexMap",
        ArrayOf::handle(new ContainersMapObject(
            std::move(uniqueKeys), std::move(uniqueValues), std::move(positions))));
}
//=============================================================================
bool
isOption(const ArrayOf& value, const std::string& expected)
{
    return isTextScalar(value) && lowerTextScalar(value) == expected;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DictionaryGateway::containersMapBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 5);
    nargoutcheck(nLhs, 0, 1);
    ClassdefHandleObject* object = getMapObject(argIn[0]);
    if (argIn.size() == 2 && isTextScalar(argIn[1])
        && lowerTextScalar(argIn[1]) == "__initnative") {
        // numeric-keyed construction: install native storage without validating
        // the (already inferred) value type, matching the interpreted builder.
        setEmptyNativeStorage(object);
        return ArrayOfVector { argIn[0] };
    }
    if ((argIn.size() == 2 || argIn.size() == 4) && isTextScalar(argIn[1])) {
        return nativeAction(object, argIn);
    } else if (argIn.size() == 1) {
        setEmptyTypedMap(object, "char", "any");
    } else if (argIn.size() == 3) {
        setCharMap(object, argIn[1], argIn[2]);
    } else if (argIn.size() == 5 && isOption(argIn[1], "keytype")
        && isOption(argIn[3], "valuetype")) {
        setEmptyTypedMap(object, lowerTextScalar(argIn[2]), lowerTextScalar(argIn[4]));
    } else {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    return ArrayOfVector { argIn[0] };
}
//=============================================================================
