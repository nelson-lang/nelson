//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include "PythonObjectHandle.hpp"
#include "PythonLibraryWrapper.hpp"
#include "characters_encoding.hpp"
#include "PyObjectHelpers.hpp"
#include "i18n.hpp"
#include "PyObjectHelpers.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonEngine.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PythonObjectHandle::PythonObjectHandle(void* _ptr)
    : HandleGenericObject(NLS_HANDLE_PYOBJECT_CATEGORY_STR, _ptr, true)
{
}
//=============================================================================
PythonObjectHandle::~PythonObjectHandle() { }
//=============================================================================
void
PythonObjectHandle::declareAsKwargs()
{
    _isKwargs = true;
}
//=============================================================================
bool
PythonObjectHandle::isKwargs()
{
    return _isKwargs;
}
//=============================================================================
void
PythonObjectHandle::display(Interface* io)
{
    std::wstring pyObjTypeName = getTypeName();
    std::wstring strFormat;

    if ((pyObjTypeName == L"dict") || (pyObjTypeName == L"str") || (pyObjTypeName == L"NoneType")) {
        strFormat = _W("  Python %s with no properties.");
    }
    if ((pyObjTypeName == L"list") || (pyObjTypeName == L"int")) {
        strFormat = _W("  Python %s with values:");
    }

    std::wstring msg = fmt::sprintf(strFormat, pyObjTypeName);
    msg.append(L"\n");
    msg.append(L"\n");
    io->outputMessage(msg);
    std::wstring rep = PyObjectToStringRepresentation(((PyObject*)this->getPointer()));
    io->outputMessage(std::wstring(L"    ") + rep + L"\n");
}
//=============================================================================
std::wstring
PythonObjectHandle::getTypeName()
{
    return TypeName(((PyObject*)this->getPointer()));
}
//=============================================================================
std::wstring
PythonObjectHandle::getClassName()
{
    std::wstring _typename = getTypeName();
    if (!_typename.empty()) {
        return utf8_to_wstring(NLS_HANDLE_PYOBJECT_CATEGORY_STR) + L"." + _typename;
    }
    return utf8_to_wstring(NLS_HANDLE_PYOBJECT_CATEGORY_STR);
}
//=============================================================================
bool
PythonObjectHandle::isMethod(const std::wstring& methodName)
{
    if (isCastMethod(methodName)) {
        return true;
    }
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
        bool callable = method && NLSPyCallable_Check(method);
        NLSPy_DECREF(method);
        return callable;
    }
    return false;
}
//=============================================================================
wstringVector
PythonObjectHandle::getCastMethods()
{
    wstringVector methodsList;
    if (getTypeName() == L"str") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"string");
    }
    return methodsList;
}
//=============================================================================
bool
PythonObjectHandle::isCastMethod(const std::wstring& methodName)
{
    wstringVector methodsList = getCastMethods();

    auto it = std::find(methodsList.begin(), methodsList.end(), methodName);
    return (it != methodsList.end());
}
//=============================================================================
wstringVector
PythonObjectHandle::getMethods()
{
    wstringVector methodsList = getCastMethods();
    PyObject* pyObject = (PyObject*)this->getPointer();
    wstringVector pythonMethodNames = getPyObjectMethods(pyObject, false);
    methodsList.insert(methodsList.end(), pythonMethodNames.begin(), pythonMethodNames.end());

    return methodsList;
}
//=============================================================================
bool
PythonObjectHandle::isProperty(const std::wstring& propertyName)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method
            = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(propertyName).c_str());
        bool callable = method && !NLSPyCallable_Check(method);
        NLSPy_DECREF(method);
        return callable;
    }
    return false;
}
//=============================================================================
wstringVector
PythonObjectHandle::getProperties()
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return getPyObjectProperties(pyObject, false);
}
//=============================================================================
bool
PythonObjectHandle::get(const std::wstring& propertyName, ArrayOf& result)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    PyObject* propertyObj
        = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(propertyName).c_str());
    if (!propertyObj || NLSPyCallable_Check(propertyObj)) {
        return false;
    }
    if (propertyObj) {
        result = PyObjectToArrayOf(propertyObj);
        NLSPy_DECREF(propertyObj);
    } else {
        NLSPy_DECREF(propertyObj);
        std::wstring errorMessage = _W("Error calling property.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage);
    }
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invoke(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    if (isCastMethod(methodName)) {
        if (methodName == L"char" && getTypeName() == L"str") {
            std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
            results << ArrayOf::characterArrayConstructor(str);
            return true;
        }
        if (methodName == L"string" && getTypeName() == L"str") {
            std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
            results << ArrayOf::stringArrayConstructor(str);
            return true;
        }
    }

    PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
    if (!method || !NLSPyCallable_Check(method)) {
        return false;
    }

    switch (inputs.size()) {
    case 0: {
        if (pyObject) {
            PyObject* method
                = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
            if (method) {
                bool callable = method && NLSPyCallable_Check(method);
                if (callable) {
                    PyObject* pyObjectResult = NLSPyObject_CallNoArgs(method);
                    NLSPy_DECREF(method);
                    if (pyObjectResult) {
                        results << PyObjectToArrayOf(pyObjectResult);
                    } else {
                        std::wstring errorMessage = _W("Error calling method.");
                        if (NLSPyErr_Occurred()) {
                            NLSPyErr_Print();
                            errorMessage = getPythonStandardError();
                            NLSPyErr_Clear();
                        }
                        Error(errorMessage);
                    }
                    return true;
                }
            }
        }
    } break;
    case 1: {
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
