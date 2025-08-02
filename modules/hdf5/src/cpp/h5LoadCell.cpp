//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5LoadCell.hpp"
#include "h5LoadVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5LoadCell(hid_t fid, const std::string& location, const std::string& variableName, bool isEmpty,
    const Dimensions& dims, ArrayOf& VariableValue)
{
    indexType nbElements = dims.getElementCount();
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[nbElements];
    } catch (const std::bad_alloc&) {
        return false;
    }
    VariableValue = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    for (indexType k = 0; k < nbElements; k++) {
        std::string elementName = std::to_string(k);
        ArrayOf value;
        if (h5LoadVariable(fid, h5path, elementName, value)) {
            elements[k] = value;
        } else {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
