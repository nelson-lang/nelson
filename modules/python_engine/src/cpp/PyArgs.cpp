//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyArgs.hpp"
#include "PythonLibraryWrapper.hpp"
#include "PythonObjectHandle.hpp"
#include "PythonEngine.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "PythonTypesWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
PyArgs(const ArrayOfVector& values)
{
    initializePythonEngine();

    ArrayOf res;

    for (size_t k = 0; k < values.size(); k = k + 2) {
        if (!values[k].isScalarStringArray() && !values[k].isRowVectorCharacterArray()) {
            Error(_W("character vector or scalar string expected."));
        }
    }
    PyObject* kwargs = NLSPyDict_New();
    if (!kwargs) {
        Error(_W("impossible to create pyargs"));
    }
    for (size_t k = 0; k < values.size(); k = k + 2) {
        std::wstring name = values[k].getContentAsWideString();
        ArrayOf value = values[k + 1];

        PyObject* pyKey = NLSPyUnicode_FromString(wstring_to_utf8(name).c_str());
        PyObject* pyValue = arrayOfToPyObject(value);
        // PyDict_SetItem(kwargs, pyKey, pyValue);

        NLSPy_DECREF(pyKey);
        NLSPy_DECREF(pyValue);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
