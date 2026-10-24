//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LibPointerObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsValidHandle.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
LibPointerObject::LibPointerObject()
    : HandleGenericObject(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, this, false)
{
    initializeCommon();
}
//=============================================================================
LibPointerObject::LibPointerObject(void* pointer)
    : HandleGenericObject(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, this, false)
{
    initializeCommon();
    _voidPointer = pointer;
}
//=============================================================================
LibPointerObject::LibPointerObject(const std::wstring& DataType)
    : HandleGenericObject(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, this, false)
{
    initializeCommon();
    if (!DynamicLinkSymbolObject::isValidDataType(DataType)) {
        Error(L"Nelson:libpointer:unsupportedType", L"Invalid argument type: %s.", DataType);
    }
    this->_DataType = DataType;
    _currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
    if (StringHelpers::ends_with(DataType, L"Ptr")) {
        _initialDimX = -1;
        _initialDimY = -1;
        _dimX = _initialDimX;
        _dimY = _initialDimY;
    } else {
        _initialDimX = 1;
        _initialDimY = 1;
        _dimX = _initialDimX;
        _dimY = _initialDimY;
        _voidPointer = ArrayOf::allocateArrayOf(_currentType, 1, stringVector(), true);
    }
}
//=============================================================================
LibPointerObject::LibPointerObject(const std::wstring& DataType, ArrayOf Value)
    : HandleGenericObject(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, this, false)
{
    initializeCommon();
    if (!DynamicLinkSymbolObject::isValidDataType(DataType)) {
        Error(L"Nelson:libpointer:unsupportedType", L"Invalid argument type: %s.", DataType);
    }
    this->_DataType = DataType;
    if (DataType == L"voidPtr") {
        _currentType = Value.dataClass();
    } else {
        _currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
    }
    _initialDimX = static_cast<long int>(Value.rows());
    _initialDimY = static_cast<long int>(Value.elementCount()) / _initialDimX;
    _dimX = _initialDimX;
    _dimY = _initialDimY;
    if (!StringHelpers::ends_with(DataType, L"Ptr")) {
        if (_currentType != Value.dataClass()) {
            Error(L"Nelson:libpointer:invalidValueType", L"Invalid #%d argument type expected:%s",
                2, DataType);
        }
    }
    if (!StringHelpers::ends_with(DataType, L"Ptr")
        && (Value.elementCount() > 1 || Value.elementCount() == 0)) {
        Error(L"Nelson:libpointer:scalarExpected", L"Invalid #%d argument scalar expected.", 2);
    }
    _voidPointer
        = ArrayOf::allocateArrayOf(Value.dataClass(), Value.elementCount(), stringVector(), true);
    memcpy(_voidPointer, Value.readWriteDataPointer(), Value.elementSize() * Value.elementCount());
}
//=============================================================================
LibPointerObject::LibPointerObject(
    void* pointer, const std::wstring& DataType, NelsonType currentType)
    : HandleGenericObject(NLS_HANDLE_LIBPOINTER_CATEGORY_STR, this, false)
{
    initializeCommon();
    this->_DataType = DataType;
    this->_voidPointer = pointer;
    this->_currentType = currentType;
}
//=============================================================================
void
LibPointerObject::initializeCommon()
{
    _propertiesNames = { L"Value", L"DataType" };
    _methodsNames = { L"disp", L"isNull", L"plus", L"reshape", L"setdatatype" };
    _DataType.clear();
    _voidPointer = nullptr;
    _initialDimX = -1;
    _initialDimY = -1;
    _dimX = _initialDimX;
    _dimY = _initialDimY;
    _currentType = NelsonType::NLS_UNKNOWN;
}
//=============================================================================
wstringVector
LibPointerObject::properties()
{
    return _propertiesNames;
}
//=============================================================================
wstringVector
LibPointerObject::methods()
{
    return _methodsNames;
}
//=============================================================================
LibPointerObject::~LibPointerObject()
{
    _propertiesNames.clear();
    _methodsNames.clear();
    _DataType.clear();
    _voidPointer = nullptr;
    _initialDimX = -1;
    _initialDimY = -1;
    _dimX = _initialDimX;
    _dimY = _initialDimY;
}
//=============================================================================
void
LibPointerObject::disp(Interface* io)
{
}
//=============================================================================
void*
LibPointerObject::pointer()
{
    return _voidPointer;
}
//=============================================================================
wstringVector
LibPointerObject::fieldnames()
{
    return _propertiesNames;
}
//=============================================================================
bool
LibPointerObject::isMethod(const std::wstring& methodName)
{
    auto it = std::find(_methodsNames.begin(), _methodsNames.end(), methodName);
    return (it != _methodsNames.end());
}
//=============================================================================
bool
LibPointerObject::isProperty(const std::wstring& propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
LibPointerObject::isWritableProperty(const std::wstring& propertyName)
{
    return false;
}
//=============================================================================
bool
LibPointerObject::isNull()
{
    return _voidPointer == nullptr;
}
//=============================================================================
LibPointerObject*
LibPointerObject::plus(indexType offset)
{
    LibPointerObject* newPtr = nullptr;
    if (_dimX == -1 || _dimY == -1 || _currentType == NLS_UNKNOWN) {
        Error(L"Nelson:libpointer:undefinedValue",
            L"The datatype and size of the value must be defined.");
    }
    if ((_dimX * _dimY) < offset) {
        Error(L"Nelson:libpointer:offsetTooLarge",
            L"Offset must not be greater than the size of the pointer.");
    }
    void* incrementedPtr = nullptr;
    if (_voidPointer == nullptr) {
        Error(L"Nelson:libpointer:nullPointerIncrement", L"null pointer cannot be incremented.");
    }
    if (_DataType == L"logical") {
        incrementedPtr = static_cast<uint8*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint8") {
        incrementedPtr = static_cast<uint8*>(_voidPointer) + offset;
    }
    if (_DataType == L"int8") {
        incrementedPtr = static_cast<int8*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint16") {
        incrementedPtr = static_cast<uint16*>(_voidPointer) + offset;
    }
    if (_DataType == L"int16") {
        incrementedPtr = static_cast<int16*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint32") {
        incrementedPtr = static_cast<uint32*>(_voidPointer) + offset;
    }
    if (_DataType == L"int32") {
        incrementedPtr = static_cast<int32*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint64") {
        incrementedPtr = static_cast<uint64*>(_voidPointer) + offset;
    }
    if (_DataType == L"int64") {
        incrementedPtr = static_cast<int64*>(_voidPointer) + offset;
    }
    if (_DataType == L"float") {
        incrementedPtr = static_cast<single*>(_voidPointer) + offset;
    }
    if (_DataType == L"single") {
        incrementedPtr = static_cast<single*>(_voidPointer) + offset;
    }
    if (_DataType == L"double") {
        incrementedPtr = static_cast<double*>(_voidPointer) + offset;
    }
    if (_DataType == L"cstring") {
        incrementedPtr = static_cast<char*>(_voidPointer) + offset;
    }
    if (_DataType == L"wstring") {
        incrementedPtr = static_cast<wchar_t*>(_voidPointer) + offset;
    }
    if (_DataType == L"void") {
        Error(L"Nelson:libpointer:cannotIncrement:voidCannotIncremented",
            L"void cannot be incremented.");
    }
    if (_DataType == L"logicalPtr") {
        incrementedPtr = static_cast<uint8*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint8Ptr") {
        incrementedPtr = static_cast<uint8*>(_voidPointer) + offset;
    }
    if (_DataType == L"int8Ptr") {
        incrementedPtr = static_cast<int8*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint16Ptr") {
        incrementedPtr = static_cast<uint16*>(_voidPointer) + offset;
    }
    if (_DataType == L"int16Ptr") {
        incrementedPtr = static_cast<int16*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint32Ptr") {
        incrementedPtr = static_cast<uint32*>(_voidPointer) + offset;
    }
    if (_DataType == L"int32Ptr") {
        incrementedPtr = static_cast<int32*>(_voidPointer) + offset;
    }
    if (_DataType == L"int64Ptr") {
        incrementedPtr = static_cast<uint64*>(_voidPointer) + offset;
    }
    if (_DataType == L"uint64Ptr") {
        incrementedPtr = static_cast<int64*>(_voidPointer) + offset;
    }
    if (_DataType == L"floatPtr") {
        incrementedPtr = static_cast<single*>(_voidPointer) + offset;
    }
    if (_DataType == L"singlePtr") {
        incrementedPtr = static_cast<single*>(_voidPointer) + offset;
    }
    if (_DataType == L"doublePtr") {
        incrementedPtr = static_cast<double*>(_voidPointer) + offset;
    }
    if (_DataType == L"voidPtr") {
        Error(L"Nelson:libpointer:cannotIncrement:voidPtrCannotIncremented",
            L"voidPtr cannot be incremented.");
    }
    if (_DataType == L"libpointer") {
        Error(L"Nelson:libpointer:cannotIncrement:libpointerCannotIncremented",
            L"libpointer cannot be incremented.");
    }
    if (incrementedPtr == nullptr) {
        Error(L"Nelson:libpointer:cannotIncrement", L"%s cannot be incremented.", _DataType);
    }
    try {
        newPtr = new LibPointerObject(incrementedPtr, _DataType, _currentType);
        newPtr->_initialDimX = _initialDimX;
        newPtr->_initialDimY = _initialDimY;
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    return newPtr;
}
//=============================================================================
std::wstring
LibPointerObject::dataType()
{
    return _DataType;
}
//=============================================================================
void
LibPointerObject::setDataType(const std::wstring& dataType)
{
    if (!DynamicLinkSymbolObject::isValidDataType(dataType)) {
        Error(L"Nelson:libpointer:invalidType", L"Invalid type.");
    }
    if (_DataType == L"voidPtr" || _DataType.empty()) {
        _currentType = DynamicLinkSymbolObject::GetNelsonType(dataType);
    } else {
        Error(L"Nelson:libpointer:incompatibleTypes", L"Incompatible types %s --> %s", _DataType,
            dataType);
    }
    _DataType = dataType;
}
//=============================================================================
void
LibPointerObject::reshape(indexType dimX, indexType dimY)
{
    if (!StringHelpers::ends_with(_DataType, L"Ptr")) {
        Error(L"Nelson:libpointer:cannotReshape", L"Only numericPtr can be reshaped.");
    }
    // Only the intended shape is recorded; get() refuses a value whose size is unknown.
    _dimX = static_cast<long int>(dimX);
    _dimY = static_cast<long int>(dimY);
}
//=============================================================================
void
LibPointerObject::get(ArrayOf& res)
{
    wstringVector fieldnames;
    ArrayOfVector fieldvalues;
    ArrayOf value;
    get(L"Value", value);
    ArrayOf datatype;
    get(L"DataType", datatype);
    fieldnames.push_back(L"Value");
    fieldvalues.push_back(value);
    fieldnames.push_back(L"DataType");
    fieldvalues.push_back(datatype);
    res = ArrayOf::structArray(fieldnames, fieldvalues);
}
//=============================================================================
bool
LibPointerObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"DataType") {
        res = ArrayOf::characterArray(_DataType);
        return true;
    }
    if (propertyName == L"Value") {
        if (_dimX == -1 || _dimY == -1 || _currentType == NLS_UNKNOWN || _voidPointer == nullptr) {
            Error(L"Nelson:libpointer:undefinedValue",
                L"The datatype and size of the value must be defined.");
        }
        // A pointer returned by a native call has no size of its own: reshape() declares
        // how many elements may be read.
        void* copyPointer
            = ArrayOf::allocateArrayOf(_currentType, _dimX * _dimY, stringVector(), true);
        if (_initialDimX != -1 && _initialDimY != -1) {
            res = ArrayOf(_currentType);
            if (_initialDimX * _initialDimY > _dimX * _dimY) {
                memcpy(copyPointer, _voidPointer, res.elementSize() * (_dimX * _dimY));
            } else {
                memcpy(
                    copyPointer, _voidPointer, res.elementSize() * (_initialDimX * _initialDimY));
            }
        } else {
            res = ArrayOf(_currentType);
            memcpy(copyPointer, _voidPointer, res.elementSize() * (_dimX * _dimY));
        }
        res = ArrayOf(_currentType, Dimensions(_dimX, _dimY), copyPointer);
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
