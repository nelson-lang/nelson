//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isClassType() const
{
    return (this->getDataClass() == NLS_CLASS_ARRAY);
}
//=============================================================================
std::string
ArrayOf::getClassType() const
{
    if (dp->dataClass != NLS_CLASS_ARRAY) {
        Error(ERROR_TYPE_CLASS_EXPECTED);
    }
    return dp->getClassTypeName();
}
//=============================================================================
void
ArrayOf::setClassType(const std::wstring& classTypeName)
{
    ArrayOf::setClassType(wstring_to_utf8(classTypeName));
}
//=============================================================================
void
ArrayOf::setClassType(const std::string& classTypeName)
{
    if (this->getDataClass() != NLS_CLASS_ARRAY) {
        Error(ERROR_TYPE_CLASS_EXPECTED);
    }
    dp->setClassTypeName(classTypeName);
}
//=============================================================================
ArrayOf
ArrayOf::classConstructor(
    const std::wstring& classTypeName, const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fieldnames;
    fieldnames.reserve(fNames.size());
    for (const auto& name : fNames) {
        fieldnames.push_back(wstring_to_utf8(name));
    }
    return classConstructor(wstring_to_utf8(classTypeName), fieldnames, values);
}
//=============================================================================
ArrayOf
ArrayOf::classConstructor(
    const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values)
{
    const ArrayOf* rptr;
    Dimensions dims;
    indexType i, j;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(
                _W("Number of field names must match number of values in structure constructor."));
        }
        dims.reset();
        dims[0] = 1;
        dims[1] = 1;
        /**
         * The dimensions of the object have been identified.  Set the
         * dimensions of the object and the field names.  Then allocate
         * the space.
         */
        qp = (ArrayOf*)allocateArrayOf(NLS_CLASS_ARRAY, dims.getElementCount(), fNames, false);
        /**
         * Work through the values, and copy the values back one at a time.
         */
        indexType length = dims.getElementCount();
        indexType offset = 0;
        for (j = 0; j < length; j++) {
            for (i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                rptr = (const ArrayOf*)rval.dp->getData();
                qp[offset] = rval;
                offset++;
            }
        }
        ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dims, qp, false, fNames);
        res.setClassType(classTypeName);
        return res;
    } catch (const Exception&) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
    return {};
}
//=============================================================================
ArrayOf
ArrayOf::emptyClassConstructor(
    const std::wstring& className, const wstringVector& fNames, Dimensions& dim)
{
    stringVector fs;
    fs.reserve(fNames.size());
    for (const auto& fName : fNames) {
        fs.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::emptyClassConstructor(wstring_to_utf8(className), fs, dim);
}
//=============================================================================
ArrayOf
ArrayOf::emptyClassConstructor(
    const std::string& className, const stringVector& fNames, Dimensions& dim)
{
    if (dim.getElementCount() != 0) {
        Error(_W("Invalid dimensions."));
    }
    ArrayOf* qp = (ArrayOf*)allocateArrayOf(NLS_CLASS_ARRAY, dim.getElementCount(), fNames, false);
    ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dim, qp, false, fNames);
    res.setClassType(className);
    return res;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
