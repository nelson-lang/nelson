//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "LibPointerObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "IsValidHandle.hpp"
#include "StringFormat.hpp"
#include "ToCellString.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
LibPointerObject::LibPointerObject()
    : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
{
    initializeCommon();
}
//=============================================================================
LibPointerObject::LibPointerObject(void* pointer)
    : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
{
    initializeCommon();
    _voidPointer = pointer;
}
//=============================================================================
LibPointerObject::LibPointerObject(std::wstring DataType)
    : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
{
    initializeCommon();
    if (!DynamicLinkSymbolObject::isValidDataType(DataType)) {
        Error(_W("Invalid argument type:") + DataType);
    }
    _DataType = DataType;
    _currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
    if (boost::algorithm::ends_with(DataType, L"Ptr")) {
        _initialDimX = -1;
        _initialDimY = -1;
        _dimX = _initialDimX;
        _dimY = _initialDimY;
    } else {
        _initialDimX = 1;
        _initialDimY = 1;
        _dimX = _initialDimX;
        _dimY = _initialDimY;
        _voidPointer = ArrayOf::allocateArrayOf(_currentType, 1);
    }
}
//=============================================================================
LibPointerObject::LibPointerObject(std::wstring DataType, ArrayOf Value)
    : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
{
    initializeCommon();
    if (!DynamicLinkSymbolObject::isValidDataType(DataType)) {
        Error(_W("Invalid argument type:") + DataType);
    }
    _DataType = DataType;
    if (DataType == L"voidPtr") {
        _currentType = Value.getDataClass();
    } else {
        _currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
    }
    Dimensions dimsValue = Value.getDimensions();
    _initialDimX = (long int)dimsValue.getRows();
    _initialDimY = (long int)dimsValue.getElementCount() / _initialDimX;
    _dimX = _initialDimX;
    _dimY = _initialDimY;
    if (!boost::algorithm::ends_with(DataType, L"Ptr")) {
        if (_currentType != Value.getDataClass()) {
            Error(_W("Invalid #2 argument type expected:") + DataType);
        }
    }
    if (!boost::algorithm::ends_with(DataType, L"Ptr")
        && (dimsValue.getElementCount() > 1 || dimsValue.getElementCount() == 0)) {
        Error(_W("Invalid #2 argument scalar expected."));
    }
    _voidPointer = ArrayOf::allocateArrayOf(Value.getDataClass(), dimsValue.getElementCount());
    memcpy(_voidPointer, Value.getReadWriteDataPointer(),
        Value.getElementSize() * dimsValue.getElementCount());
}
//=============================================================================
LibPointerObject::LibPointerObject(void* pointer, std::wstring DataType, Class currentType)
    : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
{
    initializeCommon();
    _voidPointer = pointer;
    _DataType = DataType;
    _currentType = currentType;
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
    _currentType = Nelson::Class::NLS_NOT_TYPED;
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
LibPointerObject::disp(Evaluator* eval)
{
    if (eval != nullptr) {
        Interface* io = eval->getInterface();
        if (io) {
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
void*
LibPointerObject::getPointer()
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
LibPointerObject::isMethod(std::wstring methodName)
{
    auto it = std::find(_methodsNames.begin(), _methodsNames.end(), methodName);
    return (it != _methodsNames.end());
}
//=============================================================================
bool
LibPointerObject::isProperty(std::wstring propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
LibPointerObject::isWriteableProperty(std::wstring propertyName)
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
    if (_dimX == -1 || _dimY == -1 || _currentType == NLS_NOT_TYPED) {
        Error(_W("The datatype and size of the value must be defined."));
    }
    if ((_dimX * _dimY) < offset) {
        Error(_W("Offset must not be greater than the size of the pointer."));
    }
    void* incrementedPtr = nullptr;
    if (_voidPointer == nullptr) {
        Error(_W("null pointer cannot be incremented."));
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
        Error(_W("void cannot be incremented."));
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
        Error(_W("voidPtr cannot be incremented."));
    }
    if (_DataType == L"libpointer") {
        Error(_W("libpointer cannot be incremented."));
    }
    if (incrementedPtr == nullptr) {
        Error(_DataType + L" " + _W("cannot be incremented."));
    }
    try {
        newPtr = new LibPointerObject(incrementedPtr, _DataType, _currentType);
        newPtr->_initialDimX = _initialDimX;
        newPtr->_initialDimY = _initialDimY;
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return newPtr;
}
//=============================================================================
std::wstring
LibPointerObject::getDataType()
{
    return _DataType;
}
//=============================================================================
void
LibPointerObject::setDataType(std::wstring dataType)
{
    if (!DynamicLinkSymbolObject::isValidDataType(dataType)) {
        Error(_W("Invalid type."));
    }
    if (_DataType == L"voidPtr" || _DataType == L"") {
        _currentType = DynamicLinkSymbolObject::GetNelsonType(dataType);
    } else {
        Error(_W("Incompatible types") + L" " + _DataType + L" --> " + dataType);
    }
    _DataType = dataType;
}
//=============================================================================
void
LibPointerObject::reshape(indexType dimX, indexType dimY)
{
    if (!boost::algorithm::ends_with(_DataType, L"Ptr")) {
        Error(_W("Only numericPtr can be reshaped."));
    }
    _dimX = (long int)dimX;
    _dimY = (long int)dimY;
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
    res = ArrayOf::structConstructor(fieldnames, fieldvalues);
}
//=============================================================================
bool
LibPointerObject::get(std::wstring propertyName, ArrayOf& res)
{
    if (propertyName == L"DataType") {
        res = ArrayOf::characterArrayConstructor(_DataType);
        return true;
    }
    if (propertyName == L"Value") {
        if (_dimX == -1 || _dimY == -1 || _currentType == NLS_NOT_TYPED
            || _voidPointer == nullptr) {
            Error(_W("The datatype and size of the value must be defined."));
        }
        void* copyPointer = ArrayOf::allocateArrayOf(_currentType, _dimX * _dimY);
        if (_initialDimX != -1 && _initialDimY != -1) {
            res = ArrayOf(_currentType);
            if (_initialDimX * _initialDimY > _dimX * _dimY) {
                memcpy(copyPointer, _voidPointer, res.getElementSize() * (_dimX * _dimY));
            } else {
                memcpy(copyPointer, _voidPointer,
                    res.getElementSize() * (_initialDimX * _initialDimY));
            }
        } else {
            res = ArrayOf(_currentType);
            memcpy(copyPointer, _voidPointer, res.getElementSize() * (_dimX * _dimY));
        }
        res = ArrayOf(_currentType, Dimensions(_dimX, _dimY), copyPointer);
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
