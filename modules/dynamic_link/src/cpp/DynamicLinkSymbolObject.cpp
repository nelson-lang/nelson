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
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "IsValidHandle.hpp"
#include "LibPointerObject.hpp"
#include "StringFormat.hpp"
#include "ToCellString.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/unordered_map.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
class CType
{
public:
    ffi_type* FFIType;
    Class NelsonClass;
    CType();
    CType(ffi_type* baseType, Class baseClass);
};
//=============================================================================
CType::CType() {}
//=============================================================================
CType::CType(ffi_type* baseType, Class baseClass)
{
    FFIType = baseType;
    NelsonClass = baseClass;
}
//=============================================================================
static bool ffiTypesMapInitialized = false;
static boost::unordered::unordered_map<std::wstring, CType> ffiTypesMap;
//=============================================================================
void
initializeFfiTypesMap()
{
    ffiTypesMap[L"logical"] = CType(&ffi_type_uint8, NLS_LOGICAL);
    ffiTypesMap[L"uint8"] = CType(&ffi_type_uint8, NLS_UINT8);
    ffiTypesMap[L"int8"] = CType(&ffi_type_sint8, NLS_INT8);
    ffiTypesMap[L"uint16"] = CType(&ffi_type_uint16, NLS_UINT16);
    ffiTypesMap[L"int16"] = CType(&ffi_type_sint16, NLS_INT16);
    ffiTypesMap[L"uint32"] = CType(&ffi_type_uint32, NLS_UINT32);
    ffiTypesMap[L"int32"] = CType(&ffi_type_sint32, NLS_INT32);
    ffiTypesMap[L"uint64"] = CType(&ffi_type_uint32, NLS_UINT64);
    ffiTypesMap[L"int64"] = CType(&ffi_type_sint32, NLS_INT64);
    ffiTypesMap[L"float"] = CType(&ffi_type_float, NLS_SINGLE);
    ffiTypesMap[L"single"] = CType(&ffi_type_float, NLS_SINGLE);
    ffiTypesMap[L"double"] = CType(&ffi_type_double, NLS_DOUBLE);
    ffiTypesMap[L"cstring"] = CType(&ffi_type_pointer, NLS_CHAR);
    ffiTypesMap[L"wstring"] = CType(&ffi_type_pointer, NLS_CHAR);
#ifdef NLS_INDEX_TYPE_64
    ffiTypesMap[L"void"] = CType(&ffi_type_void, NLS_UINT64);
#else
    ffiTypesMap[L"void"] = CType(&ffi_type_void, NLS_UINT32);
#endif
    ffiTypesMap[L"logicalPtr"] = CType(&ffi_type_pointer, NLS_LOGICAL);
    ffiTypesMap[L"uint8Ptr"] = CType(&ffi_type_pointer, NLS_UINT8);
    ffiTypesMap[L"int8Ptr"] = CType(&ffi_type_pointer, NLS_INT8);
    ffiTypesMap[L"uint16Ptr"] = CType(&ffi_type_pointer, NLS_UINT16);
    ffiTypesMap[L"int16Ptr"] = CType(&ffi_type_pointer, NLS_INT16);
    ffiTypesMap[L"uint32Ptr"] = CType(&ffi_type_pointer, NLS_UINT32);
    ffiTypesMap[L"int32Ptr"] = CType(&ffi_type_pointer, NLS_INT32);
    ffiTypesMap[L"int64Ptr"] = CType(&ffi_type_pointer, NLS_INT64);
    ffiTypesMap[L"uint64Ptr"] = CType(&ffi_type_pointer, NLS_UINT64);
    ffiTypesMap[L"floatPtr"] = CType(&ffi_type_pointer, NLS_SINGLE);
    ffiTypesMap[L"singlePtr"] = CType(&ffi_type_pointer, NLS_SINGLE);
    ffiTypesMap[L"doublePtr"] = CType(&ffi_type_pointer, NLS_DOUBLE);
#ifdef NLS_INDEX_TYPE_64
    ffiTypesMap[L"voidPtr"] = CType(&ffi_type_void, NLS_UINT64);
#else
    ffiTypesMap[L"voidPtr"] = CType(&ffi_type_void, NLS_UINT32);
#endif
    ffiTypesMap[L"libpointer"] = CType(&ffi_type_pointer, NLS_NOT_TYPED);
    ffiTypesMapInitialized = true;
}
//=============================================================================
static ffi_type*
GetFFIType(std::wstring type)
{
    CType ret;
    if (ffiTypesMap.count(type) != 0) {
        ret = ffiTypesMap[type];
    } else {
        Error(StringFormat(
            _W("import type %s not defined in FFI type table.").c_str(), type.c_str()));
    }
    return ret.FFIType;
}
//=============================================================================
Class
DynamicLinkSymbolObject::GetNelsonType(std::wstring type)
{
    CType ret;
    if (ffiTypesMap.count(type) != 0) {
        ret = ffiTypesMap[type];
    } else {
        Error(StringFormat(
            _W("import type %s not defined in FFI type table.").c_str(), type.c_str()));
    }
    return ret.NelsonClass;
}
//=============================================================================
DynamicLinkSymbolObject::DynamicLinkSymbolObject(ArrayOf dllibObject, void* pointerFunction,
    std::wstring symbol, std::wstring returnType, wstringVector paramsTypes)
    : HandleGenericObject(std::wstring(DLSYM_CATEGORY_STR), this, false)
{
    _propertiesNames = { L"Prototype", L"Input", L"Output" };
    if (!ffiTypesMapInitialized) {
        initializeFfiTypesMap();
    }
    _nArgIn = 0;
    _nArgOut = 0;
    _dllibObject = dllibObject;
    _pointerFunction = pointerFunction;
    _symbol = symbol;
    _returnType = returnType;
    _paramsTypes = paramsTypes;
    if (_returnType != L"void") {
        _nArgOut++;
    }
    for (std::wstring param : _paramsTypes) {
        if (param == L"void") {
            Error(_W("'void' not allowed as input type."));
        }
        if (boost::algorithm::ends_with(param, L"Ptr")) {
            _nArgOut++;
        }
    }
    _nArgIn = _paramsTypes.size();
    buildPrototype();
    ffi_type** args = (ffi_type**)malloc(sizeof(ffi_type*) * _paramsTypes.size());
    if (!args) {
        Error(_W("error memory allocation."));
    }
    int i = 0;
    for (std::wstring param : _paramsTypes) {
        args[i++] = GetFFIType(param);
    }
    if (ffi_prep_cif(
            &_cif, FFI_DEFAULT_ABI, (unsigned int)paramsTypes.size(), GetFFIType(_returnType), args)
        != FFI_OK) {
        Error(_W("Unable to import function through FFI."));
    }
}
//=============================================================================
DynamicLinkSymbolObject::~DynamicLinkSymbolObject()
{
    Dimensions dims(0, 0);
    _dllibObject = ArrayOf::emptyConstructor(dims);
    _pointerFunction = nullptr;
    _symbol = L"";
    _returnType = L"";
    _paramsTypes.clear();
    _nArgIn = 0;
    _nArgOut = 0;
    _prototype = L"";
    _propertiesNames.clear();
    _paramsInTypes.clear();
    _paramsOutTypes.clear();
}
//=============================================================================
void
DynamicLinkSymbolObject::buildPrototype()
{
    _paramsInTypes.clear();
    _paramsOutTypes.clear();
    if (_nArgOut <= 1) {
        _paramsOutTypes.push_back(_returnType);
        _prototype = _returnType + L" = " + _symbol + L" (";
    } else {
        _paramsOutTypes.push_back(_returnType);
        _prototype = L"[" + _returnType;
        for (std::wstring param : _paramsTypes) {
            if (boost::algorithm::ends_with(param, L"Ptr")) {
                _prototype = _prototype + L", " + param;
                _paramsOutTypes.push_back(param);
            }
        }
        _prototype = _prototype + L"] = " + _symbol + L" (";
    }
    bool first = true;
    for (std::wstring param : _paramsTypes) {
        if (first) {
            _prototype = _prototype + param;
            first = false;
        } else {
            _prototype = _prototype + L", " + param;
        }
        _paramsInTypes.push_back(param);
    }
    _prototype = _prototype + L")";
}
//=============================================================================
size_t
DynamicLinkSymbolObject::lengthTextToDisplay(wstringVector params)
{
    size_t len = 0;
    for (std::wstring str : params) {
        len = len + str.length() + wcslen(L", ");
    }
    return len;
}
//=============================================================================
void
DynamicLinkSymbolObject::disp(Evaluator* eval)
{
    if (eval != nullptr) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring buffer;
            io->outputMessage(L"\n");
#define PROTOTYPE_FIELD_STR L"  Prototype: "
#define OUTPUT_FIELD_STR L"  Output:    "
#define INPUT_FIELD_STR L"  Input:     "
            if (wcslen(PROTOTYPE_FIELD_STR) + _prototype.length() > io->getTerminalWidth() - 4) {
                buffer = std::wstring(PROTOTYPE_FIELD_STR) + std::wstring(L"string 1x")
                    + std::to_wstring(_prototype.length());
            } else {
                buffer = std::wstring(PROTOTYPE_FIELD_STR) + L"'" + _prototype + L"'";
            }
            io->outputMessage(buffer + L"\n");
            if (wcslen(INPUT_FIELD_STR) + lengthTextToDisplay(_paramsInTypes)
                > io->getTerminalWidth() - 4) {
                buffer = INPUT_FIELD_STR;
                buffer = buffer + std::wstring(L"1x") + std::to_wstring(_nArgIn) + L" "
                    + _W("cell array");
                io->outputMessage(buffer + L"\n");
            } else {
                buffer = INPUT_FIELD_STR;
                buffer = buffer + L"{";
                for (size_t k = 0; k < _paramsInTypes.size(); k++) {
                    if (k == _paramsInTypes.size() - 1) {
                        buffer = buffer + _paramsInTypes[k];
                    } else {
                        buffer = buffer + _paramsInTypes[k] + L", ";
                    }
                }
                buffer = buffer + L"}";
                io->outputMessage(buffer + L"\n");
            }
            if (wcslen(OUTPUT_FIELD_STR) + lengthTextToDisplay(_paramsOutTypes)
                > io->getTerminalWidth() - 4) {
                buffer = OUTPUT_FIELD_STR;
                buffer = buffer + std::wstring(L"1x") + std::to_wstring(_nArgOut) + L" "
                    + _W("cell array");
            } else {
                buffer = OUTPUT_FIELD_STR;
                buffer = buffer + L"{";
                for (size_t k = 0; k < _paramsOutTypes.size(); k++) {
                    if (k == _paramsOutTypes.size() - 1) {
                        buffer = buffer + _paramsOutTypes[k];
                    } else {
                        buffer = buffer + _paramsOutTypes[k] + L", ";
                    }
                }
                buffer = buffer + L"}";
            }
            io->outputMessage(buffer + L"\n");
            io->outputMessage(L"\n");
        }
    }
}
//=============================================================================
bool
DynamicLinkSymbolObject::isValidDataType(std::wstring DataType)
{
    if (!ffiTypesMapInitialized) {
        initializeFfiTypesMap();
    }
    return ffiTypesMap.count(DataType) != 0;
}
//=============================================================================
typedef void (*GenericFuncPointer)();
//=============================================================================
ArrayOfVector
DynamicLinkSymbolObject::call(Evaluator* eval, int nLhs, ArrayOfVector params)
{
    ArrayOfVector retval;
    ArrayOf isValidAsArray = IsValidHandle(eval, _dllibObject);
    logical isValid = isValidAsArray.getContentAsLogicalScalar();
    if (!isValid) {
        Error(_W("dllib valid handle expected."));
    }
    if (params.size() != _nArgIn) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    for (size_t k = 0; k < params.size(); k++) {
        if (GetNelsonType(_paramsTypes[k]) != params[k].getDataClass()) {
            if (params[k].getDataClass() == NLS_HANDLE) {
                if (params[k].getHandleCategory() != LIBPOINTER_CATEGORY_STR) {
                    Error(_W("libpointer handle expected."));
                }
                LibPointerObject* objLibPointer
                    = (LibPointerObject*)params[k].getContentAsHandleScalar();
                if (objLibPointer->getDataType() != _paramsTypes[k]) {
                    Error(StringFormat(
                        _W("Invalid type for #%d input argument: %ls expected.").c_str(), k + 1,
                        _paramsTypes[k].c_str()));
                }
            } else {
                Error(StringFormat(_W("Invalid type for #%d input argument: %ls expected.").c_str(),
                    k + 1, _paramsTypes[k].c_str()));
            }
        }
    }
    void** values = nullptr;
    if (params.size() > 0) {
        values = (void**)malloc(sizeof(void*) * params.size());
    }
    void** refPointers = nullptr;
    size_t nbRefPointers = 0;
    for (int i = 0; i < params.size(); i++) {
        if (boost::algorithm::ends_with(_paramsTypes[i], L"Ptr")) {
            nbRefPointers++;
        }
    }
    if (nbRefPointers > 0) {
        refPointers = (void**)malloc(sizeof(void*) * nbRefPointers);
    }
    size_t refPtrIndex = 0;
    size_t nbStrings = 0;
    for (int i = 0; i < params.size(); i++) {
        if ((_paramsTypes[i] == L"cstring") || (_paramsTypes[i] == L"wstring")) {
            nbStrings++;
        }
    }
    void** stringPointers = nullptr;
    if (nbStrings > 0) {
        stringPointers = (void**)malloc(sizeof(void*) * nbStrings);
    }
    int stringPtrIndex = 0;
    for (int i = 0; i < params.size(); i++) {
        if (params[i].getDataClass() == NLS_HANDLE) {
            if (params[i].getHandleCategory() != LIBPOINTER_CATEGORY_STR) {
                Error(_W("libpointer handle expected."));
            }
            LibPointerObject* objLibPointer
                = (LibPointerObject*)params[i].getContentAsHandleScalar();
            refPointers[refPtrIndex] = objLibPointer->getPointer();
            values[i] = &refPointers[refPtrIndex];
            refPtrIndex++;
        } else if (boost::algorithm::ends_with(_paramsTypes[i], L"Ptr")) {
            refPointers[refPtrIndex] = params[i].getReadWriteDataPointer();
            values[i] = &refPointers[refPtrIndex];
            refPtrIndex++;
        } else {
            if ((_paramsTypes[i] == L"cstring") || (_paramsTypes[i] == L"wstring")) {
                if (_paramsTypes[i] == L"cstring") {
                    stringPointers[stringPtrIndex] = params[i].getContentAsCharactersPointer();
                } else {
                    stringPointers[stringPtrIndex] = params[i].getContentAsWideCharactersPointer();
                }
                values[i] = &stringPointers[stringPtrIndex];
                stringPtrIndex++;
            } else {
                values[i] = params[i].getReadWriteDataPointer();
            }
        }
    }
    GenericFuncPointer addressFunction = (GenericFuncPointer)_pointerFunction;
    if (_returnType == L"libpointer") {
        void* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            LibPointerObject* obj = nullptr;
            try {
                obj = new LibPointerObject(returnedValue);
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            retval.push_back(ArrayOf::handleConstructor(obj));
        }
    } else if (_returnType == L"logical") {
        logical returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::logicalConstructor(returnedValue));
        }
    } else if (_returnType == L"uint8") {
        static uint8_t returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::uint8Constructor(returnedValue));
        }
    } else if (_returnType == L"int8") {
        static int8_t returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::int8Constructor(returnedValue));
        }
    } else if (_returnType == L"uint16") {
        static uint16 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::uint16Constructor(returnedValue));
        }
    } else if (_returnType == L"int16") {
        static int16 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::int16Constructor(returnedValue));
        }
    } else if (_returnType == L"uint32") {
        static uint32 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::uint32Constructor(returnedValue));
        }
    } else if (_returnType == L"int32") {
        static int32 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::int32Constructor(returnedValue));
        }
    } else if (_returnType == L"uint64") {
        static uint64 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::uint64Constructor(returnedValue));
        }
    } else if (_returnType == L"int64") {
        static int64 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::int64Constructor(returnedValue));
        }
    } else if ((_returnType == L"float") || (_returnType == L"single")) {
        static single returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::singleConstructor(returnedValue));
        }
    } else if (_returnType == L"double") {
        static double returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::doubleConstructor(returnedValue));
        }
    } else if (_returnType == L"cstring") {
        char* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::characterArrayConstructor(returnedValue));
        }
    } else if (_returnType == L"wstring") {
        wchar_t* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, values);
        if (nLhs > retval.size()) {
            retval.push_back(ArrayOf::characterArrayConstructor(returnedValue));
        }
    } else {
        int dummy;
        ffi_call(&_cif, addressFunction, &dummy, values);
    }
    int k = 0;
    for (int i = 0; i < _paramsTypes.size(); i++) {
        if (boost::algorithm::ends_with(_paramsTypes[i], L"Ptr")) {
            if (params[i].getDataClass() == NLS_HANDLE) {
                if (params[i].getHandleCategory() != LIBPOINTER_CATEGORY_STR) {
                    Error(_W("libpointer handle expected."));
                }
                LibPointerObject* objLibPointer
                    = (LibPointerObject*)params[i].getContentAsHandleScalar();
                ArrayOf retValue;
                objLibPointer->get(L"Value", retValue);
                retval.push_back(retValue);
            } else {
                void* arrayPtr = ArrayOf::allocateArrayOf(
                    params[i].getDataClass(), params[i].getDimensions().getElementCount());
                memcpy(arrayPtr, refPointers[k],
                    params[i].getDimensions().getElementCount() * params[i].getElementSize());
                retval.push_back(
                    ArrayOf(params[i].getDataClass(), params[i].getDimensions(), arrayPtr));
            }
        }
    }
    if (stringPointers != nullptr) {
        free(stringPointers);
    }
    if (refPointers != nullptr) {
        free(refPointers);
    }
    free(values);
    return retval;
}
//=============================================================================
bool
DynamicLinkSymbolObject::get(std::wstring propertyName, ArrayOf& res)
{
    if (propertyName == L"Prototype") {
        res = ArrayOf::characterArrayConstructor(_prototype);
        return true;
    }
    if (propertyName == L"Input") {
        res = ToCellStringAsRow(_paramsInTypes);
        return true;
    }
    if (propertyName == L"Output") {
        res = ToCellStringAsRow(_paramsOutTypes);
        return true;
    }
    return false;
}
//=============================================================================
bool
DynamicLinkSymbolObject::isWriteableProperty(std::wstring propertyName)
{
    return false;
}
//=============================================================================
wstringVector
DynamicLinkSymbolObject::fieldnames()
{
    return _propertiesNames;
}
//=============================================================================
bool
DynamicLinkSymbolObject::isProperty(std::wstring propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
DynamicLinkSymbolObject::isMethod(std::wstring methodName)
{
    return false;
}
//=============================================================================
}
//=============================================================================
