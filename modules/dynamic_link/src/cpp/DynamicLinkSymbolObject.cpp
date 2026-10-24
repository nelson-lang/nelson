//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <memory>
#include <unordered_map>
#include "DynamicLinkSymbolObject.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "IsValidHandle.hpp"
#include "LibPointerObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class CType
{
public:
    ffi_type* FFIType;
    NelsonType NelsonClass;
    CType();
    CType(ffi_type* baseType, NelsonType baseClass);
};
//=============================================================================
CType::CType() = default;
//=============================================================================
CType::CType(ffi_type* baseType, NelsonType baseClass) : NelsonClass(baseClass)
{
    FFIType = baseType;
}
//=============================================================================
static bool ffiTypesMapInitialized = false;
static std::unordered_map<std::wstring, CType> ffiTypesMap;
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
    ffiTypesMap[L"uint64"] = CType(&ffi_type_uint64, NLS_UINT64);
    ffiTypesMap[L"int64"] = CType(&ffi_type_sint64, NLS_INT64);
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
    ffiTypesMap[L"voidPtr"] = CType(&ffi_type_pointer, NLS_UINT64);
#else
    ffiTypesMap[L"voidPtr"] = CType(&ffi_type_pointer, NLS_UINT32);
#endif
    ffiTypesMap[L"libpointer"] = CType(&ffi_type_pointer, NLS_UNKNOWN);
    ffiTypesMapInitialized = true;
}
//=============================================================================
static ffi_type*
GetFFIType(const std::wstring& type)
{
    CType ret;
    if (ffiTypesMap.count(type) != 0) {
        ret = ffiTypesMap[type];
    } else {
        Error(L"Nelson:dlsym:importTypeNotDefined",
            L"import type %s not defined in FFI type table.", type);
    }
    return ret.FFIType;
}
//=============================================================================
NelsonType
DynamicLinkSymbolObject::GetNelsonType(const std::wstring& type)
{
    CType ret;
    if (ffiTypesMap.count(type) != 0) {
        ret = ffiTypesMap[type];
    } else {
        Error(L"Nelson:dlsym:importTypeNotDefined",
            L"import type %s not defined in FFI type table.", type);
    }
    return ret.NelsonClass;
}
//=============================================================================
DynamicLinkSymbolObject::DynamicLinkSymbolObject(const ArrayOf& dllibObject, void* pointerFunction,
    const std::wstring& symbol, const std::wstring& returnType, wstringVector paramsTypes)
    : HandleGenericObject(NLS_HANDLE_DLSYM_CATEGORY_STR, this, false)
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
    for (const std::wstring& param : _paramsTypes) {
        if (param == L"void") {
            Error(L"Nelson:dlsym:invalidPrototype", L"'void' not allowed as input type.");
        }
        if (StringHelpers::ends_with(param, L"Ptr")) {
            _nArgOut++;
        }
    }
    _nArgIn = _paramsTypes.size();
    buildPrototype();
    try {
        _ffiArgs.reserve(_paramsTypes.size());
        for (const std::wstring& param : _paramsTypes) {
            _ffiArgs.push_back(GetFFIType(param));
        }
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    if (ffi_prep_cif(&_cif, FFI_DEFAULT_ABI, (unsigned int)paramsTypes.size(),
            GetFFIType(_returnType), _ffiArgs.empty() ? nullptr : _ffiArgs.data())
        != FFI_OK) {
        Error(L"Nelson:dlsym:ffiImportFailed", L"Unable to import function through FFI.");
    }
}
//=============================================================================
DynamicLinkSymbolObject::~DynamicLinkSymbolObject()
{
    Dimensions dims(0, 0);
    _dllibObject = ArrayOf::empty(dims);
    _pointerFunction = nullptr;
    _symbol.clear();
    _returnType.clear();
    _paramsTypes.clear();
    _nArgIn = 0;
    _nArgOut = 0;
    _prototype.clear();
    _propertiesNames.clear();
    _paramsInTypes.clear();
    _paramsOutTypes.clear();
    _ffiArgs.clear();
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
        for (const std::wstring& param : _paramsTypes) {
            if (StringHelpers::ends_with(param, L"Ptr")) {
                _prototype = _prototype + L", " + param;
                _paramsOutTypes.push_back(param);
            }
        }
        _prototype = _prototype + L"] = " + _symbol + L" (";
    }
    bool first = true;
    for (const std::wstring& param : _paramsTypes) {
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
DynamicLinkSymbolObject::lengthTextToDisplay(const wstringVector& params)
{
    size_t len = 0;
    size_t postLen = wcslen(L", ");
    for (const std::wstring& str : params) {
        len = len + str.length() + postLen;
    }
    return len;
}
//=============================================================================
void
DynamicLinkSymbolObject::disp(Interface* io)
{
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
            buffer = buffer
                + formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:dynamic_link:xCellArray", L"1x%d cell array"),
                    _nArgIn);
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
            buffer = buffer
                + formatI18nMessage(Nelson::resolveMessageTemplate(
                                        L"Nelson:dynamic_link:xCellArray", L"1x%d cell array"),
                    _nArgOut);
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
    }
}
//=============================================================================
bool
DynamicLinkSymbolObject::isValidDataType(const std::wstring& DataType)
{
    if (!ffiTypesMapInitialized) {
        initializeFfiTypesMap();
    }
    return ffiTypesMap.count(DataType) != 0;
}
//=============================================================================
using GenericFuncPointer = void (*)();
//=============================================================================
ArrayOfVector
DynamicLinkSymbolObject::call(Evaluator* eval, int nLhs, ArrayOfVector params)
{
    ArrayOfVector retval;
    ArrayOf isValidAsArray = IsValidHandle(_dllibObject);
    logical isValid = isValidAsArray.asLogicalScalar();
    if (!isValid) {
        Error(L"Nelson:dynamic_link:dllibValidHandle", L"dllib valid handle expected.");
    }
    if (params.size() < _nArgIn) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    if (params.size() > _nArgIn) {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    for (size_t k = 0; k < params.size(); k++) {
        if (GetNelsonType(_paramsTypes[k]) != params[k].dataClass()) {
            if (params[k].dataClass() == NLS_HANDLE) {
                if (params[k].handleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                    Error(L"Nelson:dynamic_link:libpointerHandle", L"libpointer handle expected.");
                }
                LibPointerObject* objLibPointer = (LibPointerObject*)params[k].asHandleScalar();
                if (objLibPointer->dataType() != _paramsTypes[k]) {
                    Error(L"Nelson:dlcall:invalidArgumentType",
                        L"Invalid type for #%d argument: %s expected.", k + 1, _paramsTypes[k]);
                }
            } else {
                Error(L"Nelson:dlcall:invalidArgumentType",
                    L"Invalid type for #%d argument: %s expected.", k + 1, _paramsTypes[k]);
            }
        }
    }
    std::vector<void*> values(params.size(), nullptr);
    size_t nbRefPointers = 0;
    for (size_t i = 0; i < params.size(); i++) {
        if (params[i].dataClass() == NLS_HANDLE
            || StringHelpers::ends_with(_paramsTypes[i], L"Ptr")) {
            nbRefPointers++;
        }
    }
    std::vector<void*> refPointers(nbRefPointers, nullptr);
    std::vector<size_t> refPointerIndexes(params.size(), static_cast<size_t>(-1));
    size_t refPtrIndex = 0;
    size_t nbStrings = 0;
    for (size_t i = 0; i < params.size(); i++) {
        if ((_paramsTypes[i] == L"cstring") || (_paramsTypes[i] == L"wstring")) {
            nbStrings++;
        }
    }
    std::vector<void*> stringPointers(nbStrings, nullptr);
    std::vector<std::unique_ptr<char[]>> utf8StringStorage;
    std::vector<std::unique_ptr<wchar_t[]>> wideStringStorage;
    utf8StringStorage.reserve(nbStrings);
    wideStringStorage.reserve(nbStrings);
    size_t stringPtrIndex = 0;
    for (size_t i = 0; i < params.size(); i++) {
        if (params[i].dataClass() == NLS_HANDLE) {
            if (params[i].handleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                Error(L"Nelson:dynamic_link:libpointerHandle", L"libpointer handle expected.");
            }
            LibPointerObject* objLibPointer = (LibPointerObject*)params[i].asHandleScalar();
            refPointers[refPtrIndex] = objLibPointer->pointer();
            refPointerIndexes[i] = refPtrIndex;
            values[i] = &refPointers[refPtrIndex];
            refPtrIndex++;
        } else if (StringHelpers::ends_with(_paramsTypes[i], L"Ptr")) {
            refPointers[refPtrIndex] = params[i].readWriteDataPointer();
            refPointerIndexes[i] = refPtrIndex;
            values[i] = &refPointers[refPtrIndex];
            refPtrIndex++;
        } else {
            if ((_paramsTypes[i] == L"cstring") || (_paramsTypes[i] == L"wstring")) {
                if (_paramsTypes[i] == L"cstring") {
                    utf8StringStorage.emplace_back(params[i].toAllocatedUtf8CString());
                    stringPointers[stringPtrIndex] = utf8StringStorage.back().get();
                } else {
                    wideStringStorage.emplace_back(params[i].toAllocatedWideCString());
                    stringPointers[stringPtrIndex] = wideStringStorage.back().get();
                }
                values[i] = &stringPointers[stringPtrIndex];
                stringPtrIndex++;
            } else {
                values[i] = params[i].readWriteDataPointer();
            }
        }
    }
    GenericFuncPointer addressFunction = (GenericFuncPointer)(_pointerFunction);
    void** ffiValues = values.empty() ? nullptr : values.data();
    if (_returnType == L"libpointer") {
        void* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            LibPointerObject* obj = nullptr;
            try {
                obj = new LibPointerObject(returnedValue);
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            retval << ArrayOf::handle(obj);
        }
    } else if (_returnType == L"logical") {
        logical returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::logicalScalar(returnedValue);
        }
    } else if (_returnType == L"uint8") {
        static uint8_t returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::uint8Scalar(returnedValue);
        }
    } else if (_returnType == L"int8") {
        static int8_t returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::int8Scalar(returnedValue);
        }
    } else if (_returnType == L"uint16") {
        static uint16 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::uint16Scalar(returnedValue);
        }
    } else if (_returnType == L"int16") {
        static int16 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::int16Scalar(returnedValue);
        }
    } else if (_returnType == L"uint32") {
        static uint32 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::uint32Scalar(returnedValue);
        }
    } else if (_returnType == L"int32") {
        static int32 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::int32Scalar(returnedValue);
        }
    } else if (_returnType == L"uint64") {
        static uint64 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::uint64Scalar(returnedValue);
        }
    } else if (_returnType == L"int64") {
        static int64 returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::int64Scalar(returnedValue);
        }
    } else if ((_returnType == L"float") || (_returnType == L"single")) {
        static single returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::singleScalar(returnedValue);
        }
    } else if (_returnType == L"double") {
        static double returnedValue = 0;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::doubleScalar(returnedValue);
        }
    } else if (_returnType == L"cstring") {
        char* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::characterArray(returnedValue);
        }
    } else if (_returnType == L"wstring") {
        wchar_t* returnedValue;
        ffi_call(&_cif, addressFunction, &returnedValue, ffiValues);
        if (nLhs > (int)retval.size()) {
            retval << ArrayOf::characterArray(returnedValue);
        }
    } else {
        int dummy;
        ffi_call(&_cif, addressFunction, &dummy, ffiValues);
    }
    for (size_t i = 0; i < _paramsTypes.size(); i++) {
        if (StringHelpers::ends_with(_paramsTypes[i], L"Ptr")) {
            if (params[i].dataClass() == NLS_HANDLE) {
                if (params[i].handleCategory() != NLS_HANDLE_LIBPOINTER_CATEGORY_STR) {
                    Error(L"Nelson:dynamic_link:libpointerHandle", L"libpointer handle expected.");
                }
                LibPointerObject* objLibPointer = (LibPointerObject*)params[i].asHandleScalar();
                ArrayOf retValue;
                objLibPointer->get(L"Value", retValue);
                retval << retValue;
            } else {
                void* arrayPtr = ArrayOf::allocateArrayOf(
                    params[i].dataClass(), params[i].elementCount(), stringVector(), false);
                if (!refPointers.empty()) {
                    memcpy(arrayPtr, refPointers[refPointerIndexes[i]],
                        params[i].elementCount() * params[i].elementSize());
                }
                retval << ArrayOf(params[i].dataClass(), params[i].dimensions(), arrayPtr);
            }
        }
    }
    return retval;
}
//=============================================================================
bool
DynamicLinkSymbolObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"Prototype") {
        res = ArrayOf::characterArray(_prototype);
        return true;
    }
    if (propertyName == L"Input") {
        res = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(_paramsInTypes);
        return true;
    }
    if (propertyName == L"Output") {
        res = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(_paramsOutTypes);
        return true;
    }
    return false;
}
//=============================================================================
bool
DynamicLinkSymbolObject::isWritableProperty(const std::wstring& propertyName)
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
DynamicLinkSymbolObject::isProperty(const std::wstring& propertyName)
{
    auto it = std::find(_propertiesNames.begin(), _propertiesNames.end(), propertyName);
    return (it != _propertiesNames.end());
}
//=============================================================================
bool
DynamicLinkSymbolObject::isMethod(const std::wstring& methodName)
{
    return false;
}
//=============================================================================
wstringVector
DynamicLinkSymbolObject::properties()
{
    return _propertiesNames;
}
//=============================================================================
wstringVector
DynamicLinkSymbolObject::methods()
{
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
