//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonTypesWrapper.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PythonObjectHandle.hpp"
#include "PyObjectHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PyObject*
arrayOfToPyObject(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isHandle() && A.getHandleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        HandleGenericObject* hgo = A.getContentAsHandleScalar();
        PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
        pyObj = deepCopyPyObject((PyObject*)poh->getPointer());
    }
    return pyObj;
}
//=============================================================================
ArrayOf
PyObjectToArrayOf(PyObject* pyObject)
{
    ArrayOf res = {};
    if (!pyObject) {
        PyObject* NoneObject = &_Py_NoneStruct;
        PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(NoneObject);
        res = ArrayOf::handleConstructor(pythonObjectHandle);
    }

    PyTypeObject* pyTypeObject = Py_TYPE(pyObject);

    if (pyTypeObject == PyFloat_TypePtr) {
        double result = NLSPyFloat_AsDouble(pyObject);
        NLSPy_DECREF(pyObject);
        return ArrayOf::doubleConstructor(result);
    }
    if (pyTypeObject == PyBool_TypePtr) {
        logical value = (logical)NLSPyObject_IsTrue(pyObject);
        NLSPy_DECREF(pyObject);
        return ArrayOf::logicalConstructor(value);
    }
    if (pyTypeObject == PyComplex_TypePtr) {
        std::complex<double> value;
        Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
        NLSPy_DECREF(pyObject);
        return ArrayOf::dcomplexConstructor(pyCplx.real, pyCplx.imag);
    }

    /* if (pyTypeObject == PyLong_TypePtr) {
         double result = NLSPyLong_AsDouble(pyObject);
         NLSPy_DECREF(pyObject);
         if (NLSPyErr_Occurred()) {
             result = std::nan("NaN");
             NLSPyErr_Clear();
         }
         return ArrayOf::doubleConstructor(result);
     }
    /*   if (pyTypeObject == PyUnicode_TypePtr) {
           std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
           NLSPy_DECREF(pyObject);
           return ArrayOf::characterArrayConstructor(str);
       }

       if (pyTypeObject == PyMemoryView_TypePtr) {
           return {};
       }*/

    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(pyObject);
    return ArrayOf::handleConstructor(pythonObjectHandle);
}
//=============================================================================
}
//=============================================================================
