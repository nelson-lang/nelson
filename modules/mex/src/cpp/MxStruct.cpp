//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <algorithm>
#include "mex.h"
#include "matrix.h"
#include "MxStruct.h"
#include "MxHelpers.hpp"
#include "MxArrayOf.hpp"
//=============================================================================
mxArray*
mxCreateStructArray(mwSize ndim, const mwSize* dims, int nfields, const char** fieldnames)
{
    mxArray* ret = mxNewArray();
    if (ret != nullptr) {
        Nelson::stringVector _fieldnames;
        for (size_t k = 0; k < nfields; ++k) {
            _fieldnames.push_back(fieldnames[k]);
        }
        Nelson::Dimensions _dims;
        for (mwSize k = 0; k < ndim; ++k) {
            _dims[k] = dims[k];
        }
        auto* st = (Nelson::ArrayOf*)Nelson::ArrayOf::allocateArrayOf(
            Nelson::NLS_STRUCT_ARRAY, _dims.getElementCount(), _fieldnames, true);
        Nelson::ArrayOf s
            = Nelson::ArrayOf(Nelson::NLS_STRUCT_ARRAY, _dims, st, false, _fieldnames);

        mwSize num_dim;
        mwSize* dim_vec = GetDimensions(s, num_dim);
        ret->number_of_dims = num_dim;
        ret->dims = copyDims(num_dim, dim_vec);
        free(dim_vec);
        ret->classID = mxSTRUCT_CLASS;
        ret->issparse = false;
        ret->iscomplex = false;
        ret->imagdata = nullptr;
        ret->realdata = nullptr;
        auto* ptr = new Nelson::ArrayOf(s);
        ptr->ensureSingleOwner();
        ret->ptr = (uint64_t*)ptr;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateStructMatrix(mwSize m, mwSize n, int nfields, const char** fieldnames)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateStructArray(2, dims, nfields, fieldnames);
}
//=============================================================================
bool
mxIsStruct(const mxArray* pm)
{
    if (pm != nullptr) {
        return (pm->classID == mxSTRUCT_CLASS);
    }
    return false;
}
//=============================================================================
int
mxGetNumberOfFields(const mxArray* pm)
{
    if (mxIsStruct(pm)) {
        auto* ptr = (Nelson::ArrayOf*)pm->ptr;
        Nelson::stringVector fieldnames = ptr->getFieldNames();
        return (int)fieldnames.size();
    }
    return 0;
}
//=============================================================================
static std::string privateFieldname;
//=============================================================================
const char*
mxGetFieldNameByNumber(const mxArray* pm, int fieldnumber)
{
    if (!mxIsStruct(pm)) {
        return nullptr;
    }
    if (fieldnumber >= mxGetNumberOfFields(pm) || (fieldnumber) < 0) {
        return nullptr;
    }
    auto* ptr = (Nelson::ArrayOf*)pm->ptr;
    const auto* qp = (const Nelson::ArrayOf*)ptr->getDataPointer();
    Nelson::stringVector names = ptr->getFieldNames();
    privateFieldname = names[fieldnumber];
    return privateFieldname.c_str();
}
//=============================================================================
mxArray*
mxGetFieldByNumber(const mxArray* pm, mwIndex index, int fieldnumber)
{
    if (!mxIsStruct(pm)) {
        return nullptr;
    }
    if (index >= mxGetNumberOfElements(pm) || index < 0) {
        return nullptr;
    }
    if (fieldnumber >= mxGetNumberOfFields(pm) || fieldnumber < 0) {
        return nullptr;
    }
    auto* ptr = (Nelson::ArrayOf*)pm->ptr;
    const auto* qp = (const Nelson::ArrayOf*)ptr->getDataPointer();
    size_t fieldCount = ptr->getFieldNames().size();
    Nelson::ArrayOf field = qp[index * fieldCount + fieldnumber];
    return Nelson::ArrayOfToMxArray(field, pm->interleavedcomplex);
}
//=============================================================================
void
mxSetField(mxArray* pm, mwIndex index, const char* fieldname, mxArray* pvalue)
{
    mxSetFieldByNumber(pm, index, mxGetFieldNumber(pm, fieldname), pvalue);
}
//=============================================================================
void
mxSetFieldByNumber(mxArray* pm, mwIndex index, int fieldnumber, mxArray* pvalue)
{
    if (mxIsStruct(pm)) {
        auto* ptr = (Nelson::ArrayOf*)pm->ptr;
        Nelson::ArrayOf* sp = (Nelson::ArrayOf*)ptr->getReadWriteDataPointer();
        size_t fieldCount = ptr->getFieldNames().size();
        sp[index * fieldCount + fieldnumber] = Nelson::MxArrayToArrayOf(pvalue);
    }
}
//=============================================================================
int
mxGetFieldNumber(const mxArray* pm, const char* fieldname)
{
    if (mxIsStruct(pm)) {
        auto* ptr = (Nelson::ArrayOf*)pm->ptr;
        const auto* qp = (const Nelson::ArrayOf*)ptr->getDataPointer();
        Nelson::stringVector names = ptr->getFieldNames();
        size_t fieldCount = names.size();
        for (size_t k = 0; k < fieldCount; ++k) {
            if (strcmp(names[k].c_str(), fieldname) == 0) {
                return (int)k;
            }
        }
    }
    return -1;
}
//=============================================================================
int
mxAddField(mxArray* pm, const char* fieldname)
{
    if (mxIsStruct(pm)) {
        auto* ptr = (Nelson::ArrayOf*)pm->ptr;
        Nelson::ArrayOf* qp = (Nelson::ArrayOf*)ptr->getDataPointer();
        Nelson::stringVector names = ptr->getFieldNames();
        if (std::find(names.begin(), names.end(), std::string(fieldname)) != names.end()) {
            return mxGetFieldNumber(pm, fieldname);
        }
        return (int)ptr->insertFieldName(fieldname);
    }
    return -1;
}
//=============================================================================
void
mxRemoveField(mxArray* pm, int fieldnumber)
{
    auto* ptr = (Nelson::ArrayOf*)pm->ptr;
    Nelson::stringVector fieldnames = ptr->getFieldNames();
    size_t fieldCount = ptr->getFieldNames().size();
    if (fieldnumber >= fieldCount || fieldnumber < 0) {
        return;
    }
    Nelson::stringVector newFieldnames;
    newFieldnames.reserve(fieldnames.size() - 1);
    for (size_t k = 0; k < fieldnames.size(); ++k) {
        if (k != fieldnumber) {
            newFieldnames.push_back(fieldnames[k]);
        }
    }
    Nelson::Dimensions dims = ptr->getDimensions();
    Nelson::ArrayOf* qp = static_cast<Nelson::ArrayOf*>(Nelson::ArrayOf::allocateArrayOf(
        Nelson::NLS_STRUCT_ARRAY, dims.getElementCount(), newFieldnames, false));
    Nelson::ArrayOf st = Nelson::ArrayOf(Nelson::NLS_STRUCT_ARRAY, dims, qp, false, newFieldnames);
    for (const std::string& c : newFieldnames) {
        Nelson::ArrayOfVector data = ptr->getFieldAsList(c);
        st.setFieldAsList(c, data);
    }
    Nelson::ArrayOf* arr = new Nelson::ArrayOf(st);
    pm->ptr = (uint64_t*)arr;
}
//=============================================================================
