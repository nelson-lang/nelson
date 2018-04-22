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
    LibPointerObject::LibPointerObject(std::wstring DataType) : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
    {
        initializeCommon();
		if (!DynamicLinkSymbolObject::isValidDataType(DataType))
		{
			throw Exception(_W("Invalid argument type:") + DataType);
		}
		_DataType = DataType;
		Class classType = DynamicLinkSymbolObject::GetNelsonType(DataType);
		void *pointer = nullptr;
		Dimensions dims(0, 0);
		if (!boost::algorithm::ends_with(DataType, L"Ptr"))
		{
			pointer = ArrayOf::allocateArrayOf(classType, 0);
		}
		_value = ArrayOf(classType, dims, pointer);
    }
    //=============================================================================
    LibPointerObject::LibPointerObject(std::wstring DataType, ArrayOf Value) : HandleGenericObject(std::wstring(LIBPOINTER_CATEGORY_STR), this, false)
    {
        initializeCommon();
		if (!DynamicLinkSymbolObject::isValidDataType(DataType))
		{
			throw Exception(_W("Invalid argument type:") + DataType);
		}
		Class classType = DynamicLinkSymbolObject::GetNelsonType(DataType);
		if (DataType == L"voidPtr")
		{

		}
		else
		{
			if (classType != Value.getDataClass())
			{
				throw Exception(_W("Invalid #2 argument type expected:") + DataType);
			}
		}
		if (!boost::algorithm::ends_with(DataType, L"Ptr") && (!Value.isScalar() && !Value.isEmpty()))
		{
			throw Exception(_W("#2 argument must be scalar."));
		}
		_DataType = DataType;
		_value = Value;
		_value.ensureSingleOwner();
	}
    //=============================================================================
    void LibPointerObject::initializeCommon()
    {
        _propertiesNames = { L"Value", L"DataType" };
        _methodsNames = { L"disp", L"isNull", L"plus", L"reshape", L"setdatatype" };
        _DataType.clear();
    }
    //=============================================================================
    LibPointerObject::~LibPointerObject()
    {
        _propertiesNames.clear();
        _methodsNames.clear();
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
        return _value.getReadWriteDataPointer();
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
		if (_DataType == L"")
		{
			return true;
		}
		return _value.isEmpty();
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
    void LibPointerObject::reshape(size_t dimX, size_t dimY)
    {
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
			res = _value;
			return true;
		}
		return false;
	}
	//=============================================================================
}
//=============================================================================
