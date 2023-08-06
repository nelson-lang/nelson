//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadFunctionHandle.hpp"
#include "h5LoadStruct.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadFunctionHandle(hid_t fid, const std::string& location, const std::string& variableName,
    bool isEmpty, const Dimensions& dims, ArrayOf& VariableValue)
{
    bool bSuccess = false;
    ArrayOf value;
    if (h5LoadStruct(fid, location, variableName, isEmpty, dims, false, "", value)) {
        ArrayOf nameStr = value.getField("name");
        ArrayOf anonymousStr = value.getField("anonymous");
        function_handle fh;
        fh.name = nameStr.getContentAsCString();
        if (!fh.name.empty()) {
            fh.anonymousHandle = nullptr;
        } else {
            fh.anonymousHandle
                = (nelson_handle*)new AnonymousMacroFunctionDef(anonymousStr.getContentAsCString());
        }
        VariableValue = ArrayOf::functionHandleConstructor(fh);
        bSuccess = true;
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
