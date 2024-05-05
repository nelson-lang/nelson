//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyIsEqual.hpp"
#include "Error.hpp"
#include "PythonObjectHandle.hpp"
#include "PyObjectHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
numPyIsEqual(PyObject* pyObjectA, PyObject* pyObjectB, bool negate, bool& result);
//=============================================================================
bool
PyIsEqual(const ArrayOf& A, const ArrayOf& B)
{
    if (A.getDataClass() != B.getDataClass()) {
        return false;
    }
    if (A.getHandleCategory() != B.getHandleCategory()) {
        return false;
    }
    if (A.getHandleCategory() != NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        return false;
    }
    if (!A.isScalar() && !B.isScalar()) {
        return false;
    }
    PythonObjectHandle* pyObjectA = (PythonObjectHandle*)A.getContentAsHandleScalar();
    PythonObjectHandle* pyObjectB = (PythonObjectHandle*)B.getContentAsHandleScalar();
    PyObject* p1 = (PyObject*)pyObjectA->getPointer();
    PyObject* p2 = (PyObject*)pyObjectB->getPointer();

    bool res = false;
    if (numPyIsEqual(p1, p2, false, res)) {
        return res;
    }
    res = (NLSPyObject_RichCompareBool(p1, p2, Py_EQ) == 1);
    NLSPyErr_Clear();
    return res;
}
//=============================================================================
static bool
numPyIsEqual(PyObject* pyObjectA, PyObject* pyObjectB, bool negate, bool& result)
{
    result = false;
    std::wstring typeA = TypeName(pyObjectA);
    std::wstring typeB = TypeName(pyObjectB);
    if (StringHelpers::starts_with(typeA, L"numpy.")
        || StringHelpers::starts_with(typeB, L"numpy.")) {
        PyObject* numpy_module = NLSPyImport_ImportModule("numpy");
        if (numpy_module == nullptr) {
            NLSPyErr_Clear();
            return false;
        }
        PyObject* args = NLSPy_BuildValue("(OO)", pyObjectA, pyObjectB);
        PyObject* array_equal_func = NLSPyObject_GetAttrString(numpy_module, "array_equal");
        if (array_equal_func == nullptr) {
            NLSPy_DECREF(numpy_module);
            return false;
        }
        PyObject* result_object = NLSPyObject_CallObject(array_equal_func, (PyObject*)args);
        PyTypeObject* pyTypeObject = Py_TYPE(result_object);
        if (pyTypeObject == &PyBool_Type) {
            result = negate ? !(logical)NLSPyObject_IsTrue(result_object)
                            : (logical)NLSPyObject_IsTrue(result_object);
        }
        NLSPy_DECREF(numpy_module);
        NLSPy_DECREF(array_equal_func);
        NLSPy_DECREF(result_object);
        return true;
    }
    return false;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
