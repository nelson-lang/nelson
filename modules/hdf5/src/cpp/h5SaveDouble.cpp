//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include "h5SaveDouble.hpp"
#include "h5SaveInteger.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "Exception.hpp"
#include "SparseDynamicFunctions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
h5SaveDoubleEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue);
//=============================================================================
static bool
h5SaveDoubleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression);
//=============================================================================
static bool
h5SaveSparseDoubleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression);
//=============================================================================
bool
h5SaveDouble(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    if (VariableValue.isEmpty(false)) {
        bSuccess = h5SaveDoubleEmptyMatrix(fid, location, variableName, VariableValue);
    } else {
        if (VariableValue.isSparse()) {
            bSuccess = h5SaveSparseDoubleMatrix(
                fid, location, variableName, VariableValue, useCompression);
        } else {
            bSuccess
                = h5SaveDoubleMatrix(fid, location, variableName, VariableValue, useCompression);
        }
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveDoubleEmptyMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    double value = 0;
    hid_t type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
    hsize_t dimsAsHsize_t[1];
    dimsAsHsize_t[0] = 1;
    hid_t dspace_id = H5Screate_simple((int)1, dimsAsHsize_t, dimsAsHsize_t);
    if (dspace_id < 0) {
        return false;
    }

    hid_t dataset_id
        = H5Dcreate(fid, h5path.c_str(), type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dataset_id < 0) {
        return false;
    }
    herr_t status = H5Dwrite(dataset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &value);

    H5Dclose(dataset_id);
    H5Sclose(dspace_id);
    if (status < 0) {
        return false;
    }
    bSuccess = h5SaveEmptyAttribute(fid, h5path);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (bSuccess) {
        if (VariableValue.isComplex()) {
            bSuccess = h5SaveComplexAttribute(fid, h5path);
        }
        if (bSuccess) {
            bSuccess = h5SaveDimensionsAttribute(fid, h5path, VariableValue.getDimensions());
            if (bSuccess && VariableValue.isSparse()) {
                h5SaveSparseAttribute(fid, h5path);
                bSuccess = h5SaveUint64Attribute(fid, h5path, NELSON_SPARSE_NZMAX_STR, (uint64)1);
            }
        }
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveDoubleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    hid_t dspace_id = H5I_INVALID_HID;
    hid_t type_id = H5Tcopy(H5T_NATIVE_DOUBLE);
    Dimensions dimsValue = VariableValue.getDimensions();

    hsize_t* dimsAsHsize_t = nullptr;
    indexType nbElementsSizeData;
    if (dimsValue.isScalar()) {
        try {
            dimsAsHsize_t = new_with_exception<hsize_t>(1, true);
        } catch (Exception&) {
            return false;
        }
        nbElementsSizeData = 1;
        dimsAsHsize_t[0] = 1;
        dspace_id = H5Screate_simple((int)1, dimsAsHsize_t, dimsAsHsize_t);
    } else {
        try {
            dimsAsHsize_t = new_with_exception<hsize_t>(dimsValue.getLength(), true);
        } catch (Exception&) {
            return false;
        }
        nbElementsSizeData = dimsValue.getLength();
        for (indexType k = 1; k <= nbElementsSizeData; k++) {
            dimsAsHsize_t[k - 1] = (hsize_t)dimsValue[nbElementsSizeData - k];
        }
        dspace_id = H5Screate_simple((int)dimsValue.getLength(), dimsAsHsize_t, dimsAsHsize_t);
    }
    delete[] dimsAsHsize_t;

    void* buffer = const_cast<void*>(VariableValue.getDataPointer());
    if (VariableValue.isComplex()) {
        using complex_type = struct complex_type
        {
            double r;
            double i;
        };
        hid_t compoundId = H5Tcreate(H5T_COMPOUND, sizeof(doublecomplex));
        H5Tinsert(compoundId, "real", HOFFSET(complex_type, r), H5T_NATIVE_DOUBLE);
        H5Tinsert(compoundId, "imag", HOFFSET(complex_type, i), H5T_NATIVE_DOUBLE);
        hid_t plist = setCompression(dimsValue, useCompression);
        hid_t dataset_id = H5Dcreate(
            fid, h5path.c_str(), compoundId, dspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
        herr_t status = H5Dwrite(dataset_id, compoundId, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
        H5Pclose(plist);
        H5Dclose(dataset_id);
        H5Sclose(dspace_id);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = h5SaveComplexAttribute(fid, h5path);
        }
    } else {
        hid_t plist = setCompression(dimsValue, useCompression);
        hid_t dataset_id
            = H5Dcreate(fid, h5path.c_str(), type_id, dspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
        herr_t status = H5Dwrite(dataset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer);
        H5Pclose(plist);
        H5Dclose(dataset_id);
        H5Sclose(dspace_id);
        if (status < 0) {
            bSuccess = false;
        } else {
            bSuccess = true;
        }
    }

    if (bSuccess) {
        bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
        if (!bSuccess) {
            return false;
        }
        bSuccess = h5SaveDimensionsAttribute(fid, h5path, dimsValue);
    }
    return bSuccess;
}
//=============================================================================
bool
h5SaveSparseDoubleMatrix(hid_t fid, const std::string& location, const std::string& variableName,
    const ArrayOf& VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }
    h5LDeleteIfExists(fid, h5path);

    Dimensions dims = VariableValue.getDimensions();
    Eigen::SparseMatrix<double, 0, signedIndexType>* spmat
        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)VariableValue.getSparseDataPointer();
    indexType nnz = 0;
    if (spmat) {
        nnz = spmat->nonZeros();
    }

    indexType* ptrI = nullptr;
    indexType* ptrJ = nullptr;
    try {
        ptrI = new indexType[nnz];
        ptrJ = new indexType[nnz];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    int nz = 0;
    void* ptrV = SparseToIJVDynamicFunction(VariableValue.getDataClass(), dims.getRows(),
        dims.getColumns(), VariableValue.getSparseDataPointer(), ptrI, ptrJ, nz);

    uint64* pI
        = static_cast<uint64*>(ArrayOf::allocateArrayOf(NLS_UINT64, nnz, stringVector(), false));
    for (indexType k = 0; k < nnz; k++) {
        pI[k] = static_cast<uint64>(ptrI[k]);
    }
    ArrayOf I = ArrayOf(NLS_UINT64, Dimensions(1, nnz), (void*)pI);
    delete[] ptrI;

    uint64* pJ
        = static_cast<uint64*>(ArrayOf::allocateArrayOf(NLS_UINT64, nnz, stringVector(), false));
    for (indexType k = 0; k < nnz; k++) {
        pJ[k] = static_cast<uint64>(ptrJ[k]);
    }
    ArrayOf J = ArrayOf(NLS_UINT64, Dimensions(1, nnz), (void*)pJ);
    delete[] ptrJ;

    hid_t gcpl = H5Pcreate(H5P_GROUP_CREATE);
    hid_t group = H5Gcreate(fid, variableName.c_str(), H5P_DEFAULT, gcpl, H5P_DEFAULT);
    herr_t status = H5Gclose(group);
    if (status < 0) {
        return false;
    }

    std::string rootPath = location + variableName + std::string("/");

    std::string dataName = std::string("data");
    ArrayOf V;
    if (VariableValue.isComplex()) {
        V = ArrayOf(NLS_DCOMPLEX, Dimensions(1, nnz), ptrV);
    } else {
        V = ArrayOf(VariableValue.getDataClass(), Dimensions(1, nnz), ptrV);
    }
    bSuccess = h5SaveDouble(fid, rootPath, dataName, V, useCompression);
    if (!bSuccess) {
        return false;
    }

    std::string irName = std::string("ir");
    bSuccess = h5SaveInteger(fid, rootPath, irName, I, useCompression);
    if (!bSuccess) {
        return false;
    }

    std::string jcName = std::string("jc");
    bSuccess = h5SaveInteger(fid, rootPath, jcName, J, useCompression);
    if (!bSuccess) {
        return false;
    }

    bSuccess = h5SaveUint64Attribute(
        fid, rootPath, NELSON_SPARSE_NZMAX_STR, (uint64)spmat->data().allocatedSize());
    if (!bSuccess) {
        return false;
    }

    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }

    if (VariableValue.isComplex()) {
        bSuccess = h5SaveComplexAttribute(fid, h5path);
    }

    if (bSuccess) {
        bSuccess = h5SaveSparseAttribute(fid, h5path);
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
