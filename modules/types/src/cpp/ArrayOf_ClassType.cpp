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
#include <utility>
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isClassType() const
{
    return (this->dataClass() == NLS_CLASS_ARRAY || this->dataClass() == NLS_CLASSDEF_ARRAY);
}
//=============================================================================
bool
ArrayOf::isClassdefType() const
{
    return this->dataClass() == NLS_CLASSDEF_ARRAY;
}
//=============================================================================
std::string
ArrayOf::classType() const
{
    if (!isClassType()) {
        Error(L"Nelson:types:classExpected", L"class expected.");
    }
    return dp->classTypeName();
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
    if (this->dataClass() != NLS_CLASS_ARRAY && this->dataClass() != NLS_CLASSDEF_ARRAY) {
        Error(L"Nelson:types:classExpected", L"class expected.");
    }
    // The name is stored on the shared Data node: detach before mutating.
    ensureSingleOwner();
    dp->setClassTypeName(classTypeName);
}
//=============================================================================
ClassdefLayoutPtr
ArrayOf::classdefLayout() const
{
    if (!isClassdefType()) {
        return nullptr;
    }
    return dp->classdefLayout();
}
//=============================================================================
void
ArrayOf::setClassdefLayout(ClassdefLayoutPtr layout)
{
    if (!isClassdefType()) {
        return;
    }
    ensureSingleOwner();
    dp->setClassdefLayout(std::move(layout));
}
//=============================================================================
ArrayOf
ArrayOf::classArray(
    const std::wstring& classTypeName, const wstringVector& fNames, const ArrayOfVector& values)
{
    stringVector fieldnames;
    fieldnames.reserve(fNames.size());
    for (const auto& name : fNames) {
        fieldnames.push_back(wstring_to_utf8(name));
    }
    return classArray(wstring_to_utf8(classTypeName), fieldnames, values);
}
//=============================================================================
ArrayOf
ArrayOf::classArray(
    const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values)
{
    Dimensions dims;
    indexType i, j;
    ArrayOf* qp = nullptr;
    try {
        if (fNames.size() != values.size()) {
            Error(L"Nelson:types:fieldNamesValuesCountMismatch",
                L"Number of field names must match number of values in %s constructor.",
                L"structure");
        }
        dims.reset();
        dims[0] = 1;
        dims[1] = 1;
        qp = (ArrayOf*)allocateArrayOf(NLS_CLASS_ARRAY, dims.elementCount(), fNames, false);
        indexType length = dims.elementCount();
        indexType offset = 0;
        for (j = 0; j < length; j++) {
            for (i = 0; i < (indexType)fNames.size(); i++) {
                ArrayOf rval = values[i];
                qp[offset] = rval;
                offset++;
            }
        }
        ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dims, qp, false, fNames);
        res.setClassType(classTypeName);
        return res;
    } catch (const NelsonException&) {
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
ArrayOf::emptyClass(const std::wstring& className, const wstringVector& fNames, Dimensions& dim)
{
    stringVector fs;
    fs.reserve(fNames.size());
    for (const auto& fName : fNames) {
        fs.push_back(wstring_to_utf8(fName));
    }
    return ArrayOf::emptyClass(wstring_to_utf8(className), fs, dim);
}
//=============================================================================
ArrayOf
ArrayOf::emptyClass(const std::string& className, const stringVector& fNames, Dimensions& dim)
{
    if (dim.elementCount() != 0) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    ArrayOf* qp = (ArrayOf*)allocateArrayOf(NLS_CLASS_ARRAY, dim.elementCount(), fNames, false);
    ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dim, qp, false, fNames);
    res.setClassType(className);
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::classdefArray(
    const std::string& classTypeName, const stringVector& fNames, const ArrayOfVector& values)
{
    if (fNames.size() != values.size()) {
        Error(L"Nelson:types:fieldNamesValuesCountMismatch",
            L"Number of field names must match number of values in %s constructor.", L"classdef");
    }
    Dimensions dims(1, 1);
    ArrayOf* qp = nullptr;
    try {
        qp = static_cast<ArrayOf*>(
            allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fNames, false));
        for (indexType i = 0; i < static_cast<indexType>(fNames.size()); i++) {
            qp[i] = values[i];
        }
        ArrayOf res = ArrayOf(NLS_CLASSDEF_ARRAY, dims, qp, false, fNames);
        res.setClassType(classTypeName);
        return res;
    } catch (const NelsonException&) {
        delete[] qp;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::emptyClassdef(const std::string& className, const stringVector& fNames, Dimensions& dim)
{
    if (dim.elementCount() != 0) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    ArrayOf* qp = static_cast<ArrayOf*>(
        allocateArrayOf(NLS_CLASSDEF_ARRAY, dim.elementCount(), fNames, false));
    ArrayOf res = ArrayOf(NLS_CLASSDEF_ARRAY, dim, qp, false, fNames);
    res.setClassType(className);
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
