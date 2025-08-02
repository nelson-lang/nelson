//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mexextBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Validators.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MexGateway::mexextBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    if (argIn.size() == 0) {
        std::wstring currentMexExt = getMexExtension();
        if (currentMexExt.empty()) {
            Error(_W("mex extension not yet supported."));
        }
        retval << ArrayOf::characterArrayConstructor(currentMexExt);
        return retval;
    }
    mustBeTextScalar(argIn, 0);
    std::wstring value = argIn[0].getContentAsWideString();
    if (value != L"all") {
        Error(_W("Input must be 'all' or \"all\"."));
    }

    wstringVector extensions
        = { L"nexglx", L"nexa64", L"nexmaci", L"nexmaci64", L"nexmacm1", L"nexw32", L"nexw64" };
    wstringVector archs
        = { L"glnx86", L"glnxa64", L"maci", L"maci64", L"macm1", L"win32", L"win64" };

    ArrayOfVector ext(extensions.size());
    ArrayOfVector arch(extensions.size());
    for (indexType k = 0; k < extensions.size(); ++k) {
        ext.push_back(ArrayOf::characterArrayConstructor(extensions[k]));
        arch.push_back(ArrayOf::characterArrayConstructor(archs[k]));
    }
    Dimensions dims(extensions.size(), 1);
    stringVector fieldnames(2);
    fieldnames[0] = "ext";
    fieldnames[1] = "arch";
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
    ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    st.setFieldAsList(fieldnames[0], ext);
    st.setFieldAsList(fieldnames[1], arch);
    retval << st;
    return retval;
}
//=============================================================================
