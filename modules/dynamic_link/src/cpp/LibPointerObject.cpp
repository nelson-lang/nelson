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
#include <boost/algorithm/string.hpp>
#include "LibPointerObject.hpp"
#include "StringFormat.hpp"
#include "IsValidHandle.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "DynamicLinkSymbolObject.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    LibPointerObject::LibPointerObject() : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
    {
        initializeCommon();
    }
	//=============================================================================
	LibPointerObject::LibPointerObject(void *pointer) : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
	{
		initializeCommon();
		_voidPointer = pointer;
	}
	//=============================================================================
    LibPointerObject::LibPointerObject(std::wstring DataType) : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
    {
        initializeCommon();
		if (!DynamicLinkSymbolObject::isValidDataType(DataType))
		{
			throw Exception(_W("Invalid argument type:") + DataType);
		}
		_DataType = DataType;
		_currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
		if (boost::algorithm::ends_with(DataType, L"Ptr"))
		{
			_dimX = -1;
			_dimY = -1;
		}
		else
		{
			_dimX = 1;
			_dimY = 1;
			_voidPointer = ArrayOf::allocateArrayOf(_currentType, 1);
		}
    }
    //=============================================================================
    LibPointerObject::LibPointerObject(std::wstring DataType, ArrayOf Value) : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
    {
		initializeCommon();
		if (!DynamicLinkSymbolObject::isValidDataType(DataType))
		{
			throw Exception(_W("Invalid argument type:") + DataType);
		}
		_DataType = DataType;
		if (DataType == L"voidPtr")
		{
			_currentType = Value.getDataClass();
		}
		else
		{
			_currentType = DynamicLinkSymbolObject::GetNelsonType(DataType);
		}
		Dimensions dimsValue = Value.getDimensions();
		_dimX = (long int)dimsValue.getRows();
		_dimY = (long int)dimsValue.getElementCount() / _dimX;

		if (!boost::algorithm::ends_with(DataType, L"Ptr"))
		{
			if (_currentType != Value.getDataClass())
			{
				throw Exception(_W("Invalid #2 argument type expected:") + DataType);
			}
		}
		if (!boost::algorithm::ends_with(DataType, L"Ptr") && (dimsValue.getElementCount() > 1 || dimsValue.getElementCount() == 0))
		{
			throw Exception(_W("Invalid #2 argument scalar expected."));
		}
		_voidPointer = ArrayOf::allocateArrayOf(Value.getDataClass(), dimsValue.getElementCount());
		memcpy(_voidPointer, Value.getReadWriteDataPointer(), Value.getElementSize() * dimsValue.getElementCount());
	}
    //=============================================================================
    void LibPointerObject::initializeCommon()
    {
        _propertiesNames = { L"Value", L"DataType" };
        _methodsNames = { L"disp", L"isNull", L"plus", L"reshape", L"setdatatype" };

		_DataType.clear();
		_voidPointer = nullptr;
		_dimX = -1;
		_dimY = -1;
		_currentType = Nelson::Class::NLS_NOT_TYPED;
    }
    //=============================================================================
    LibPointerObject::~LibPointerObject()
    {
        _propertiesNames.clear();
        _methodsNames.clear();
		_DataType.clear();
		_voidPointer = nullptr;
		_dimX = -1;
		_dimY = -1;
	}
    //=============================================================================
    void LibPointerObject::disp(Evaluator *eval)
    {
        if (eval != nullptr)
        {
            Interface *io = eval->getInterface();
            if (io)
            {
                io->outputMessage(L"\n");
            }
        }
    }
    //=============================================================================
    void *LibPointerObject::getPointer()
    {
		return _voidPointer;
    }
    //=============================================================================
    wstringVector LibPointerObject::fieldnames()
    {
        return _propertiesNames;
    }
    //=============================================================================
    bool LibPointerObject::isproperty(std::wstring propertyName)
    {
        auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
        return (it != _propertiesNames.end());
    }
    //=============================================================================
    bool LibPointerObject::isWriteableProperty(std::wstring propertyName)
    {
        return false;
    }
    //=============================================================================
    bool LibPointerObject::isNull()
    {
		return _voidPointer == nullptr;
    }
    //=============================================================================
    bool LibPointerObject::plus()
    {
        return false;
    }
    //=============================================================================
    std::wstring LibPointerObject::getDataType()
    {
        return _DataType;
    }
	//=============================================================================
	void LibPointerObject::setDataType(std::wstring dataType)
	{
		if (!DynamicLinkSymbolObject::isValidDataType(dataType))
		{
			throw Exception(_W("Invalid type."));
		}
		_currentType = DynamicLinkSymbolObject::GetNelsonType(dataType);
		_DataType = dataType;
	}
    //=============================================================================
    void LibPointerObject::reshape(size_t dimX, size_t dimY)
    {
		_dimX = (long int)dimX;
		_dimY = (long int)dimY;
    }
    //=============================================================================
	void LibPointerObject::get(ArrayOf &res)
	{
		if (_dimX == -1 || _dimY == -1 || _currentType == NLS_NOT_TYPED)
		{
			throw Exception(_W("The datatype and size of the value must be defined."));
		}
		wstringVector fieldnames;
		ArrayOfVector  fieldvalues;
		fieldnames.push_back(L"Value");
		fieldnames.push_back(L"DataType");
		void *copyPointer = ArrayOf::allocateArrayOf(_currentType, _dimX * _dimY);
		ArrayOf value = ArrayOf(_currentType);
		memcpy(copyPointer, _voidPointer, value.getElementSize() * (_dimX * _dimY));
		value = ArrayOf(_currentType, Dimensions(_dimX, _dimY), copyPointer);
		fieldvalues.push_back(value);
		fieldvalues.push_back(ArrayOf::stringConstructor(_DataType));
		res = ArrayOf::structConstructor(fieldnames, fieldvalues);
	}
	//=============================================================================
	bool LibPointerObject::get(std::wstring propertyName, ArrayOf &res)
	{
		if (propertyName == L"DataType")
		{
			res = ArrayOf::stringConstructor(_DataType);
			return true;
		}
		if (propertyName == L"Value")
		{
			if (_dimX == -1 || _dimY == -1 || _currentType == NLS_NOT_TYPED)
			{
				throw Exception(_W("The datatype and size of the value must be defined."));
			}
			void *copyPointer = ArrayOf::allocateArrayOf(_currentType, _dimX * _dimY);
			res = ArrayOf(_currentType);
			memcpy(copyPointer, _voidPointer, res.getElementSize() * (_dimX * _dimY));
			res = ArrayOf(_currentType, Dimensions(_dimX, _dimY), copyPointer);
			return true;
		}
		return false;
	}
	//=============================================================================
}
//=============================================================================
