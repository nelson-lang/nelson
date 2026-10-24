//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "StructConcatenationFields.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
reorderStructFields(const ArrayOf& value, const stringVector& fieldnames);
//=============================================================================
ArrayOf
matchStructFields(const ArrayOf& value, const stringVector& fieldnames)
{
    stringVector valueFieldnames = value.fieldNames();
    if (valueFieldnames == fieldnames) {
        return value;
    }
    // Both messages share the identifier: raised text-keyed.
    if (valueFieldnames.size() != fieldnames.size()) {
        RaiseError(_("Number of fields in structure arrays being concatenated do not match. "
                     "Concatenation of structure arrays requires that these arrays have the same "
                     "set of fields."),
            "Nelson:catenate:structFieldBad");
    }
    for (const std::string& name : fieldnames) {
        if (std::find(valueFieldnames.begin(), valueFieldnames.end(), name)
            == valueFieldnames.end()) {
            RaiseError(_("Names of fields in structure arrays being concatenated do not match. "
                         "Concatenation of structure arrays requires that these arrays have the "
                         "same set of fields."),
                "Nelson:catenate:structFieldBad");
        }
    }
    return reorderStructFields(value, fieldnames);
}
//=============================================================================
static ArrayOf
reorderStructFields(const ArrayOf& value, const stringVector& fieldnames)
{
    Dimensions dims = value.dimensions();
    if (dims.elementCount() == 0) {
        return ArrayOf::emptyStruct(fieldnames, dims);
    }
    void* data = ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false);
    ArrayOf result(NLS_STRUCT_ARRAY, dims, data, false, fieldnames);
    for (const std::string& name : fieldnames) {
        ArrayOfVector values = value.fieldAsList(name);
        result.setFieldAsList(name, values);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
