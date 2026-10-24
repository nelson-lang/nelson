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
#include <filesystem>
#include <map>
#include <numeric>
#include <string>
#include <type_traits>
#include <vector>
#include <netcdf.h>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "NetcdfConstants.hpp"
#include "NetcdfDispatcher.hpp"
#include "NetcdfTypes.hpp"
#include "NetcdfSchemaHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    static void
    checkNc(int status);
    static std::string
    asString(const ArrayOf& value);
    static int
    asInt(const ArrayOf& value);
    static size_t
    asSize(const ArrayOf& value);
    static std::wstring
    toWide(const std::string& value);
    static ArrayOf
    dscalar(double v);
    static ArrayOf
    lscalar(bool v);
    static ArrayOf
    text(const std::string& v);
    static ArrayOf
    rowDoubles(const std::vector<double>& values);
    static ArrayOf
    rowStrings(const std::vector<std::string>& values);
    static ArrayOf
    cellArray(const std::vector<ArrayOf>& values);
    static ArrayOf
    structArray(const stringVector& names, const std::vector<ArrayOfVector>& rows);
    static ArrayOf
    structFieldAt(const ArrayOf& value, indexType index, const std::string& field);
    static std::vector<int>
    intVector(const ArrayOf& value);
    static std::vector<size_t>
    sizeVector(const ArrayOf& value, bool highLevelStart);
    static std::vector<ptrdiff_t>
    strideVector(const ArrayOf& value);
    static Dimensions
    dimsFromShape(const std::vector<size_t>& shape);
    static std::vector<size_t>
    shapeOfVar(int ncid, int varid);
    template <typename T>
    static ArrayOf
    readAs(int ncid, int varid, nc_type xtype, const Dimensions& dims, const size_t* start,
        const size_t* count, const ptrdiff_t* stride);
    static ArrayOf
    readVar(int ncid, int varid, const std::vector<size_t>& start = {},
        const std::vector<size_t>& count = {}, const std::vector<ptrdiff_t>& stride = {});
    static void
    writeVar(int ncid, int varid, const ArrayOf& value, const std::vector<size_t>& start = {},
        const std::vector<size_t>& count = {}, const std::vector<ptrdiff_t>& stride = {});
    static ArrayOf
    readAtt(int ncid, int varid, const std::string& name);
    static void
    writeAtt(int ncid, int varid, const std::string& name, const ArrayOf& value);
    static ArrayOf
    fileInfo(int ncid, const std::string& filename);
    static int
    varIdByName(int ncid, const std::string& name);
    static int
    openExistingOrCreate(const std::string& filename);
    static void
    highCreate(const ArrayOfVector& args);
    static ArrayOfVector
    highLevel(Evaluator* eval, int nLhs, const std::string& op, const ArrayOfVector& args);
    static ArrayOfVector
    lowLevel(int nLhs, const std::string& op, const ArrayOfVector& args);
    //=============================================================================
} // namespace
//=============================================================================
namespace NetcdfGateway {
    //=============================================================================
    ArrayOfVector
    netcdfDispatcher(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
    {
        nargincheck(argIn, 1);
        std::string op = asString(argIn[0]);
        if (op.rfind("netcdf.", 0) == 0) {
            return lowLevel(nLhs, op, argIn);
        }
        return highLevel(eval, nLhs, op, argIn);
    }
    //=============================================================================
} // namespace NetcdfGateway
//=============================================================================
namespace {
    //=============================================================================
    static void
    checkNc(int status)
    {
        if (status != NC_NOERR) {
            RaiseError(utf8_to_wstring(std::string("netCDF: ") + nc_strerror(status)),
                L"Nelson:netcdf:libraryError");
        }
    }
    //=============================================================================
    static std::string
    asString(const ArrayOf& value)
    {
        return wstring_to_utf8(value.asWideString());
    }
    //=============================================================================
    static int
    asInt(const ArrayOf& value)
    {
        return static_cast<int>(value.asDoubleScalar(true, true));
    }
    //=============================================================================
    static size_t
    asSize(const ArrayOf& value)
    {
        double v = value.asDoubleScalar(true, true);
        if (v < 0) {
            Error(L"Nelson:netcdf:mustBeNonnegativeInteger",
                L"Expected a non-negative integer value.");
        }
        return static_cast<size_t>(v);
    }
    //=============================================================================
    static std::wstring
    toWide(const std::string& value)
    {
        return utf8_to_wstring(value);
    }
    //=============================================================================
    static ArrayOf
    dscalar(double v)
    {
        return ArrayOf::doubleScalar(v);
    }
    //=============================================================================
    static ArrayOf
    lscalar(bool v)
    {
        return ArrayOf::logicalScalar(v);
    }
    //=============================================================================
    static ArrayOf
    text(const std::string& v)
    {
        return ArrayOf::characterArray(toWide(v));
    }
    //=============================================================================
    static ArrayOf
    rowDoubles(const std::vector<double>& values)
    {
        Dimensions dims(1, static_cast<indexType>(values.size()));
        double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
        for (size_t k = 0; k < values.size(); ++k) {
            data[k] = values[k];
        }
        return ArrayOf(NLS_DOUBLE, dims, data);
    }
    //=============================================================================
    static ArrayOf
    rowStrings(const std::vector<std::string>& values)
    {
        return ArrayOf::cellArrayFromArrayOfCharacterRowVectors(values);
    }
    //=============================================================================
    static ArrayOf
    cellArray(const std::vector<ArrayOf>& values)
    {
        Dimensions dims(1, static_cast<indexType>(values.size()));
        ArrayOf* data
            = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size()));
        for (size_t k = 0; k < values.size(); ++k) {
            data[k] = values[k];
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, data);
    }
    //=============================================================================
    static ArrayOf
    structArray(const stringVector& names, const std::vector<ArrayOfVector>& rows)
    {
        if (rows.empty()) {
            Dimensions dims(0, 0);
            return ArrayOf::emptyStruct(names, dims);
        }
        ArrayOfVector values;
        for (size_t field = 0; field < names.size(); ++field) {
            std::vector<ArrayOf> column;
            for (const auto& row : rows) {
                column.push_back(row[field]);
            }
            values << cellArray(column);
        }
        return ArrayOf::structArray(names, values);
    }
    //=============================================================================
    static ArrayOf
    structFieldAt(const ArrayOf& value, indexType index, const std::string& field)
    {
        if (!value.isStruct()) {
            Error(L"Nelson:netcdf:structExpected", L"Structure expected.");
        }
        int64 fieldIndex = value.fieldIndex(field);
        if (fieldIndex < 0) {
            Error(L"Nelson:netcdf:schemaFieldNotFound", L"Schema field not found.");
        }
        const ArrayOf* data = static_cast<const ArrayOf*>(value.dataPointer());
        return data[index * value.fieldNames().size() + static_cast<indexType>(fieldIndex)];
    }
    //=============================================================================
    static std::vector<int>
    intVector(const ArrayOf& value)
    {
        std::vector<double> raw = value.asDoubleVector();
        std::vector<int> out;
        out.reserve(raw.size());
        for (double v : raw) {
            out.push_back(static_cast<int>(v));
        }
        return out;
    }
    //=============================================================================
    static std::vector<size_t>
    sizeVector(const ArrayOf& value, bool highLevelStart)
    {
        std::vector<double> raw = value.asDoubleVector();
        std::vector<size_t> out;
        out.reserve(raw.size());
        for (double v : raw) {
            if (highLevelStart) {
                if (v < 1) {
                    Error(L"Nelson:netcdf:invalidStart", L"Start values must be positive.");
                }
                out.push_back(static_cast<size_t>(v - 1));
            } else {
                if (v < 0) {
                    Error(L"Nelson:netcdf:invalidIndex", L"Expected non-negative index values.");
                }
                out.push_back(static_cast<size_t>(v));
            }
        }
        return out;
    }
    //=============================================================================
    static std::vector<ptrdiff_t>
    strideVector(const ArrayOf& value)
    {
        std::vector<double> raw = value.asDoubleVector();
        std::vector<ptrdiff_t> out;
        out.reserve(raw.size());
        for (double v : raw) {
            if (v < 1) {
                Error(L"Nelson:netcdf:invalidStride", L"Stride values must be positive.");
            }
            out.push_back(static_cast<ptrdiff_t>(v));
        }
        return out;
    }
    //=============================================================================
    static Dimensions
    dimsFromShape(const std::vector<size_t>& shape)
    {
        if (shape.empty()) {
            return Dimensions(1, 1);
        }
        if (shape.size() == 1) {
            return Dimensions(1, static_cast<indexType>(shape[0]));
        }
        std::vector<indexType> d;
        d.reserve(shape.size());
        for (size_t v : shape) {
            d.push_back(static_cast<indexType>(v));
        }
        return Dimensions(d);
    }
    //=============================================================================
    static std::vector<size_t>
    shapeOfVar(int ncid, int varid)
    {
        char name[NC_MAX_NAME + 1];
        nc_type xtype;
        int ndims = 0;
        int dimids[NC_MAX_VAR_DIMS];
        int natts = 0;
        checkNc(nc_inq_var(ncid, varid, name, &xtype, &ndims, dimids, &natts));
        std::vector<size_t> shape(ndims);
        for (int k = 0; k < ndims; ++k) {
            checkNc(nc_inq_dimlen(ncid, dimids[k], &shape[k]));
        }
        return shape;
    }
    //=============================================================================
    template <typename T>
    static ArrayOf
    readAs(int ncid, int varid, nc_type xtype, const Dimensions& dims, const size_t* start,
        const size_t* count, const ptrdiff_t* stride)
    {
        size_t n = dims.elementCount();
        T* data
            = static_cast<T*>(ArrayOf::allocateArrayOf(NetcdfGateway::netcdfNelsonType(xtype), n));
        int status;
        if (start && count && stride) {
            if constexpr (std::is_same_v<T, double>) {
                status = nc_get_vars_double(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, single>) {
                status = nc_get_vars_float(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, int8>) {
                status = nc_get_vars_schar(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, uint8>) {
                status = nc_get_vars_uchar(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, int16>) {
                status = nc_get_vars_short(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, uint16>) {
                status = nc_get_vars_ushort(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, int32>) {
                status = nc_get_vars_int(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, uint32>) {
                status = nc_get_vars_uint(ncid, varid, start, count, stride, data);
            } else if constexpr (std::is_same_v<T, int64>) {
                status = nc_get_vars_longlong(
                    ncid, varid, start, count, stride, reinterpret_cast<long long*>(data));
            } else {
                status = nc_get_vars_ulonglong(
                    ncid, varid, start, count, stride, reinterpret_cast<unsigned long long*>(data));
            }
        } else {
            if constexpr (std::is_same_v<T, double>) {
                status = nc_get_var_double(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, single>) {
                status = nc_get_var_float(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, int8>) {
                status = nc_get_var_schar(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, uint8>) {
                status = nc_get_var_uchar(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, int16>) {
                status = nc_get_var_short(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, uint16>) {
                status = nc_get_var_ushort(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, int32>) {
                status = nc_get_var_int(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, uint32>) {
                status = nc_get_var_uint(ncid, varid, data);
            } else if constexpr (std::is_same_v<T, int64>) {
                status = nc_get_var_longlong(ncid, varid, reinterpret_cast<long long*>(data));
            } else {
                status = nc_get_var_ulonglong(
                    ncid, varid, reinterpret_cast<unsigned long long*>(data));
            }
        }
        checkNc(status);
        return ArrayOf(NetcdfGateway::netcdfNelsonType(xtype), dims, data);
    }
    //=============================================================================
    static ArrayOf
    readVar(int ncid, int varid, const std::vector<size_t>& start, const std::vector<size_t>& count,
        const std::vector<ptrdiff_t>& stride)
    {
        char name[NC_MAX_NAME + 1];
        nc_type xtype;
        int ndims = 0;
        int dimids[NC_MAX_VAR_DIMS];
        int natts = 0;
        checkNc(nc_inq_var(ncid, varid, name, &xtype, &ndims, dimids, &natts));
        std::vector<size_t> shape = count.empty() ? shapeOfVar(ncid, varid) : count;
        Dimensions dims = dimsFromShape(shape);
        const size_t* pStart = start.empty() ? nullptr : start.data();
        const size_t* pCount = count.empty() ? nullptr : count.data();
        const ptrdiff_t* pStride = stride.empty() ? nullptr : stride.data();
        if (xtype == NC_CHAR) {
            std::vector<char> bytes(dims.elementCount() + 1, '\0');
            checkNc(pStart ? nc_get_vars_text(ncid, varid, pStart, pCount, pStride, bytes.data())
                           : nc_get_var_text(ncid, varid, bytes.data()));
            return text(std::string(bytes.data()));
        }
        switch (xtype) {
        case NC_FLOAT:
            return readAs<single>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_BYTE:
            return readAs<int8>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_UBYTE:
            return readAs<uint8>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_SHORT:
            return readAs<int16>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_USHORT:
            return readAs<uint16>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_INT:
            return readAs<int32>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_UINT:
            return readAs<uint32>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_INT64:
            return readAs<int64>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        case NC_UINT64:
            return readAs<uint64>(ncid, varid, xtype, dims, pStart, pCount, pStride);
        default:
            return readAs<double>(ncid, varid, NC_DOUBLE, dims, pStart, pCount, pStride);
        }
    }
    //=============================================================================
    static void
    writeVar(int ncid, int varid, const ArrayOf& value, const std::vector<size_t>& start,
        const std::vector<size_t>& count, const std::vector<ptrdiff_t>& stride)
    {
        const void* data = value.dataPointer();
        NelsonType cls = value.dataClass();
        const size_t* pStart = start.empty() ? nullptr : start.data();
        const size_t* pCount = count.empty() ? nullptr : count.data();
        const ptrdiff_t* pStride = stride.empty() ? nullptr : stride.data();
        if (cls == NLS_CHAR) {
            std::string s = asString(value);
            checkNc(pStart ? nc_put_vars_text(ncid, varid, pStart, pCount, pStride, s.c_str())
                           : nc_put_var_text(ncid, varid, s.c_str()));
            return;
        }
        if (pStart) {
            if (cls == NLS_SINGLE) {
                checkNc(nc_put_vars_float(
                    ncid, varid, pStart, pCount, pStride, static_cast<const single*>(data)));
            } else if (cls == NLS_INT8) {
                checkNc(nc_put_vars_schar(
                    ncid, varid, pStart, pCount, pStride, static_cast<const int8*>(data)));
            } else if (cls == NLS_UINT8 || cls == NLS_LOGICAL) {
                checkNc(nc_put_vars_uchar(
                    ncid, varid, pStart, pCount, pStride, static_cast<const uint8*>(data)));
            } else if (cls == NLS_INT16) {
                checkNc(nc_put_vars_short(
                    ncid, varid, pStart, pCount, pStride, static_cast<const int16*>(data)));
            } else if (cls == NLS_UINT16) {
                checkNc(nc_put_vars_ushort(
                    ncid, varid, pStart, pCount, pStride, static_cast<const uint16*>(data)));
            } else if (cls == NLS_INT32) {
                checkNc(nc_put_vars_int(
                    ncid, varid, pStart, pCount, pStride, static_cast<const int32*>(data)));
            } else if (cls == NLS_UINT32) {
                checkNc(nc_put_vars_uint(
                    ncid, varid, pStart, pCount, pStride, static_cast<const uint32*>(data)));
            } else if (cls == NLS_INT64) {
                checkNc(nc_put_vars_longlong(ncid, varid, pStart, pCount, pStride,
                    reinterpret_cast<const long long*>(data)));
            } else if (cls == NLS_UINT64) {
                checkNc(nc_put_vars_ulonglong(ncid, varid, pStart, pCount, pStride,
                    reinterpret_cast<const unsigned long long*>(data)));
            } else {
                checkNc(nc_put_vars_double(
                    ncid, varid, pStart, pCount, pStride, static_cast<const double*>(data)));
            }
        } else {
            if (cls == NLS_SINGLE) {
                checkNc(nc_put_var_float(ncid, varid, static_cast<const single*>(data)));
            } else if (cls == NLS_INT8) {
                checkNc(nc_put_var_schar(ncid, varid, static_cast<const int8*>(data)));
            } else if (cls == NLS_UINT8 || cls == NLS_LOGICAL) {
                checkNc(nc_put_var_uchar(ncid, varid, static_cast<const uint8*>(data)));
            } else if (cls == NLS_INT16) {
                checkNc(nc_put_var_short(ncid, varid, static_cast<const int16*>(data)));
            } else if (cls == NLS_UINT16) {
                checkNc(nc_put_var_ushort(ncid, varid, static_cast<const uint16*>(data)));
            } else if (cls == NLS_INT32) {
                checkNc(nc_put_var_int(ncid, varid, static_cast<const int32*>(data)));
            } else if (cls == NLS_UINT32) {
                checkNc(nc_put_var_uint(ncid, varid, static_cast<const uint32*>(data)));
            } else if (cls == NLS_INT64) {
                checkNc(nc_put_var_longlong(ncid, varid, reinterpret_cast<const long long*>(data)));
            } else if (cls == NLS_UINT64) {
                checkNc(nc_put_var_ulonglong(
                    ncid, varid, reinterpret_cast<const unsigned long long*>(data)));
            } else {
                checkNc(nc_put_var_double(ncid, varid, static_cast<const double*>(data)));
            }
        }
    }
    //=============================================================================
    static ArrayOf
    readAtt(int ncid, int varid, const std::string& name)
    {
        return NetcdfGateway::netcdfReadAtt(ncid, varid, name);
    }
    //=============================================================================
    static void
    writeAtt(int ncid, int varid, const std::string& name, const ArrayOf& value)
    {
        if (value.dataClass() == NLS_CHAR) {
            std::string s = asString(value);
            checkNc(nc_put_att_text(ncid, varid, name.c_str(), s.size(), s.c_str()));
            return;
        }
        size_t n = value.elementCount();
        nc_type xtype = NetcdfGateway::netcdfTypeFromArray(value);
        checkNc(nc_put_att(ncid, varid, name.c_str(), xtype, n, value.dataPointer()));
    }
    //=============================================================================
    static ArrayOf
    fileInfo(int ncid, const std::string& filename)
    {
        int ndims = 0;
        int nvars = 0;
        int ngatts = 0;
        int unlim = -1;
        checkNc(nc_inq(ncid, &ndims, &nvars, &ngatts, &unlim));
        int format = 0;
        checkNc(nc_inq_format(ncid, &format));
        std::vector<double> dimids;
        std::vector<double> varids;
        for (int k = 0; k < ndims; ++k) {
            dimids.push_back(k);
        }
        for (int k = 0; k < nvars; ++k) {
            varids.push_back(k);
        }
        std::vector<ArrayOfVector> dimRows;
        for (int k = 0; k < ndims; ++k) {
            char name[NC_MAX_NAME + 1];
            size_t len = 0;
            checkNc(nc_inq_dim(ncid, k, name, &len));
            ArrayOfVector row;
            row << text(name) << dscalar(static_cast<double>(len)) << lscalar(k == unlim)
                << dscalar(k);
            dimRows.push_back(row);
        }
        std::vector<ArrayOfVector> attRows;
        for (int k = 0; k < ngatts; ++k) {
            char attName[NC_MAX_NAME + 1];
            checkNc(nc_inq_attname(ncid, NC_GLOBAL, k, attName));
            ArrayOfVector row;
            row << text(attName) << readAtt(ncid, NC_GLOBAL, attName);
            attRows.push_back(row);
        }
        std::vector<ArrayOfVector> varRows;
        for (int k = 0; k < nvars; ++k) {
            char name[NC_MAX_NAME + 1];
            nc_type xtype;
            int nd = 0, natts = 0, ids[NC_MAX_VAR_DIMS];
            checkNc(nc_inq_var(ncid, k, name, &xtype, &nd, ids, &natts));
            std::vector<double> dims;
            std::vector<double> sizeValues;
            std::vector<ArrayOfVector> varDimRows;
            for (int j = 0; j < nd; ++j) {
                char dimName[NC_MAX_NAME + 1];
                size_t dimLen = 0;
                checkNc(nc_inq_dim(ncid, ids[j], dimName, &dimLen));
                dims.push_back(static_cast<double>(ids[j]));
                sizeValues.push_back(static_cast<double>(dimLen));
                ArrayOfVector dimRow;
                dimRow << text(dimName) << dscalar(static_cast<double>(dimLen))
                       << lscalar(ids[j] == unlim) << dscalar(ids[j]);
                varDimRows.push_back(dimRow);
            }
            std::vector<ArrayOfVector> varAttRows;
            for (int j = 0; j < natts; ++j) {
                char attName[NC_MAX_NAME + 1];
                checkNc(nc_inq_attname(ncid, k, j, attName));
                if (std::string(attName) == "_FillValue") {
                    continue;
                }
                ArrayOfVector attRow;
                attRow << text(attName) << readAtt(ncid, k, attName);
                varAttRows.push_back(attRow);
            }
            int storage = 0;
            std::vector<double> chunkValues;
            std::vector<size_t> chunks(static_cast<size_t>(nd));
            if (nd > 0 && nc_inq_var_chunking(ncid, k, &storage, chunks.data()) == NC_NOERR
                && storage == NC_CHUNKED) {
                for (size_t value : chunks) {
                    chunkValues.push_back(static_cast<double>(value));
                }
            }
            int shuffle = 0;
            int deflate = 0;
            int deflateLevel = 0;
            nc_inq_var_deflate(ncid, k, &shuffle, &deflate, &deflateLevel);
            int fletcher32 = 0;
            nc_inq_var_fletcher32(ncid, k, &fletcher32);
            int noFill = 0;
            ArrayOf fillValue = NetcdfGateway::netcdfReadFill(ncid, k, xtype, noFill);
            ArrayOfVector row;
            row << text(name) << dscalar(static_cast<double>(xtype))
                << text(NetcdfGateway::netcdfTypeName(xtype)) << rowDoubles(dims)
                << dscalar(static_cast<double>(natts)) << dscalar(k) << rowDoubles(sizeValues)
                << structArray({ "Name", "Length", "Unlimited", "ID" }, varDimRows)
                << structArray({ "Name", "Value" }, varAttRows) << rowDoubles(chunkValues)
                << dscalar(storage) << dscalar(deflate ? deflateLevel : 0) << lscalar(shuffle != 0)
                << lscalar(fletcher32 != 0) << fillValue << lscalar(noFill != 0);
            varRows.push_back(row);
        }
        ArrayOfVector values;
        values << text(filename) << text("/") << text(NetcdfGateway::netcdfFormatName(format))
               << dscalar(ndims) << dscalar(nvars) << dscalar(ngatts) << dscalar(unlim)
               << rowDoubles(dimids) << rowDoubles(varids)
               << structArray({ "Name", "Length", "Unlimited", "ID" }, dimRows)
               << structArray({ "Name", "DatatypeID", "Datatype", "DimensionIDs", "NumAttributes",
                                  "ID", "Size", "Dimensions", "Attributes", "ChunkSize", "Storage",
                                  "DeflateLevel", "Shuffle", "Fletcher32", "FillValue", "NoFill" },
                      varRows)
               << structArray({ "Name", "Value" }, attRows) << NetcdfGateway::netcdfGroupInfo(ncid);
        return ArrayOf::structScalar(
            { "Filename", "Name", "Format", "NumDimensions", "NumVariables", "NumGlobalAttributes",
                "UnlimitedDimensionID", "DimensionIDs", "VariableIDs", "Dimensions", "Variables",
                "Attributes", "Groups" },
            values);
    }
    //=============================================================================
    static int
    varIdByName(int ncid, const std::string& name)
    {
        int varid = 0;
        checkNc(nc_inq_varid(ncid, name.c_str(), &varid));
        return varid;
    }
    //=============================================================================
    static int
    openExistingOrCreate(const std::string& filename)
    {
        int ncid = 0;
        if (std::filesystem::exists(filename)) {
            checkNc(nc_open(filename.c_str(), NC_WRITE, &ncid));
            checkNc(nc_redef(ncid));
        } else {
            checkNc(nc_create(filename.c_str(), NC_CLOBBER | NC_NETCDF4, &ncid));
        }
        return ncid;
    }
    //=============================================================================
    static void
    highCreate(const ArrayOfVector& args)
    {
        if (args.size() < 3) {
            Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
        }
        std::string filename = asString(args[1]);
        std::string varname = asString(args[2]);
        nc_type xtype = NC_DOUBLE;
        std::vector<std::pair<std::string, size_t>> dims;
        for (size_t k = 3; k + 1 < args.size(); k += 2) {
            std::string key = NetcdfGateway::netcdfLowerName(asString(args[k]));
            if (key == "datatype") {
                xtype = NetcdfGateway::netcdfTypeFromName(asString(args[k + 1]));
            } else if (key == "dimensions") {
                const ArrayOf& cell = args[k + 1];
                if (!cell.isCell()) {
                    Error(L"Nelson:netcdf:invalidDimensions", L"Dimensions must be a cell array.");
                }
                const ArrayOf* items = static_cast<const ArrayOf*>(cell.dataPointer());
                size_t n = cell.elementCount();
                for (size_t i = 0; i + 1 < n; i += 2) {
                    dims.push_back({ asString(items[i]), asSize(items[i + 1]) });
                }
            }
        }
        if (dims.empty()) {
            Error(L"Nelson:netcdf:dimensionsRequired", L"Dimensions must be specified.");
        }
        int ncid = openExistingOrCreate(filename);
        std::vector<int> dimids;
        for (auto& d : dims) {
            int dimid = 0;
            int status = nc_inq_dimid(ncid, d.first.c_str(), &dimid);
            if (status != NC_NOERR) {
                checkNc(nc_def_dim(ncid, d.first.c_str(), d.second, &dimid));
            }
            dimids.push_back(dimid);
        }
        int varid = 0;
        checkNc(nc_def_var(
            ncid, varname.c_str(), xtype, static_cast<int>(dimids.size()), dimids.data(), &varid));
        checkNc(nc_enddef(ncid));
        checkNc(nc_close(ncid));
    }
    //=============================================================================
    static ArrayOfVector
    highLevel(Evaluator* eval, int nLhs, const std::string& op, const ArrayOfVector& args)
    {
        ArrayOfVector out;
        if (op == "nccreate") {
            nargoutcheck(nLhs, 0, 0);
            highCreate(args);
            return out;
        }
        if (op == "ncwrite") {
            nargoutcheck(nLhs, 0, 0);
            nargincheck(args, 4, 6);
            int ncid = 0;
            checkNc(nc_open(asString(args[1]).c_str(), NC_WRITE, &ncid));
            int varid = varIdByName(ncid, asString(args[2]));
            if (args.size() > 4) {
                std::vector<size_t> start = sizeVector(args[4], true);
                std::vector<size_t> count(1, static_cast<size_t>(args[3].elementCount()));
                std::vector<ptrdiff_t> stride = args.size() > 5
                    ? strideVector(args[5])
                    : std::vector<ptrdiff_t>(start.size(), 1);
                writeVar(ncid, varid, args[3], start, count, stride);
            } else {
                writeVar(ncid, varid, args[3]);
            }
            checkNc(nc_close(ncid));
            return out;
        }
        if (op == "ncread") {
            nargoutcheck(nLhs, 0, 1);
            nargincheck(args, 3, 5);
            int ncid = 0;
            checkNc(nc_open(asString(args[1]).c_str(), NC_NOWRITE, &ncid));
            int varid = varIdByName(ncid, asString(args[2]));
            if (args.size() > 3) {
                std::vector<size_t> start = sizeVector(args[3], true);
                std::vector<size_t> count
                    = args.size() > 4 ? sizeVector(args[4], false) : shapeOfVar(ncid, varid);
                std::vector<ptrdiff_t> stride(start.size(), 1);
                out << readVar(ncid, varid, start, count, stride);
            } else {
                out << readVar(ncid, varid);
            }
            checkNc(nc_close(ncid));
            return out;
        }
        if (op == "ncwriteatt" || op == "ncreadatt") {
            bool write = op == "ncwriteatt";
            nargoutcheck(nLhs, 0, write ? 0 : 1);
            nargincheck(args, write ? 5 : 4, write ? 5 : 4);
            int ncid = 0;
            checkNc(nc_open(asString(args[1]).c_str(), write ? NC_WRITE : NC_NOWRITE, &ncid));
            std::string loc = asString(args[2]);
            int varid = (loc == "/" || loc.empty()) ? NC_GLOBAL : varIdByName(ncid, loc);
            if (write) {
                writeAtt(ncid, varid, asString(args[3]), args[4]);
            } else {
                out << readAtt(ncid, varid, asString(args[3]));
            }
            checkNc(nc_close(ncid));
            return out;
        }
        if (op == "ncinfo") {
            nargoutcheck(nLhs, 0, 1);
            nargincheck(args, 2, 3);
            int ncid = 0;
            checkNc(nc_open(asString(args[1]).c_str(), NC_NOWRITE, &ncid));
            out << fileInfo(ncid, asString(args[1]));
            checkNc(nc_close(ncid));
            return out;
        }
        if (op == "ncwriteschema") {
            nargoutcheck(nLhs, 0, 0);
            nargincheck(args, 3, 3);
            std::string filename = asString(args[1]);
            const ArrayOf& schema = args[2];
            if (!schema.isStruct()) {
                Error(L"Nelson:netcdf:invalidSchema", L"Schema must be a scalar structure.");
            }
            int ncid = 0;
            checkNc(nc_create(filename.c_str(), NC_CLOBBER | NC_NETCDF4, &ncid));
            NetcdfGateway::netcdfWriteSchemaInto(ncid, schema);
            checkNc(nc_close(ncid));
            return out;
        }
        Error(L"Nelson:netcdf:unsupportedOperation", L"Unsupported netCDF operation.");
        return out;
    }
    //=============================================================================
    static ArrayOfVector
    lowLevel(int nLhs, const std::string& op, const ArrayOfVector& args)
    {
        ArrayOfVector out;
        if (op == "netcdf.getConstant") {
            nargincheck(args, 2, 2);
            nargoutcheck(nLhs, 0, 1);
            auto it = NetcdfGateway::netcdfConstants().find(asString(args[1]));
            if (it == NetcdfGateway::netcdfConstants().end()) {
                Error(L"Nelson:netcdf:unknownConstant", L"Unknown netCDF constant.");
            }
            out << dscalar(it->second);
        } else if (op == "netcdf.getConstantNames") {
            nargincheck(args, 1, 1);
            nargoutcheck(nLhs, 0, 1);
            std::vector<std::string> names;
            for (auto& it : NetcdfGateway::netcdfConstants()) {
                names.push_back(it.first);
            }
            out << rowStrings(names);
        } else if (op == "netcdf.inqLibVers") {
            nargincheck(args, 1, 1);
            out << text(nc_inq_libvers());
        } else if (op == "netcdf.create") {
            nargincheck(args, 3, 3);
            int ncid = 0;
            checkNc(nc_create(asString(args[1]).c_str(), asInt(args[2]), &ncid));
            out << dscalar(ncid);
        } else if (op == "netcdf.open") {
            nargincheck(args, 3, 3);
            int ncid = 0;
            checkNc(nc_open(asString(args[1]).c_str(), asInt(args[2]), &ncid));
            out << dscalar(ncid);
        } else if (op == "netcdf.close") {
            nargincheck(args, 2, 2);
            checkNc(nc_close(asInt(args[1])));
        } else if (op == "netcdf.abort") {
            nargincheck(args, 2, 2);
            checkNc(nc_abort(asInt(args[1])));
        } else if (op == "netcdf.endDef") {
            nargincheck(args, 2, 2);
            checkNc(nc_enddef(asInt(args[1])));
        } else if (op == "netcdf.reDef") {
            nargincheck(args, 2, 2);
            checkNc(nc_redef(asInt(args[1])));
        } else if (op == "netcdf.sync") {
            nargincheck(args, 2, 2);
            checkNc(nc_sync(asInt(args[1])));
        } else if (op == "netcdf.setFill") {
            nargincheck(args, 3, 3);
            int old = 0;
            checkNc(nc_set_fill(asInt(args[1]), asInt(args[2]), &old));
            out << dscalar(old);
        } else if (op == "netcdf.inq") {
            nargincheck(args, 2, 2);
            int nd = 0, nv = 0, na = 0, ud = -1;
            checkNc(nc_inq(asInt(args[1]), &nd, &nv, &na, &ud));
            out << dscalar(nd) << dscalar(nv) << dscalar(na) << dscalar(ud);
        } else if (op == "netcdf.inqFormat") {
            nargincheck(args, 2, 2);
            int fmt = 0;
            checkNc(nc_inq_format(asInt(args[1]), &fmt));
            out << dscalar(fmt);
        } else if (op == "netcdf.defDim") {
            nargincheck(args, 4, 4);
            int id = 0;
            checkNc(nc_def_dim(asInt(args[1]), asString(args[2]).c_str(), asSize(args[3]), &id));
            out << dscalar(id);
        } else if (op == "netcdf.inqDim") {
            nargincheck(args, 3, 3);
            char name[NC_MAX_NAME + 1];
            size_t len = 0;
            checkNc(nc_inq_dim(asInt(args[1]), asInt(args[2]), name, &len));
            out << text(name) << dscalar(static_cast<double>(len));
        } else if (op == "netcdf.inqDimID") {
            nargincheck(args, 3, 3);
            int id = 0;
            checkNc(nc_inq_dimid(asInt(args[1]), asString(args[2]).c_str(), &id));
            out << dscalar(id);
        } else if (op == "netcdf.renameDim") {
            nargincheck(args, 4, 4);
            checkNc(nc_rename_dim(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str()));
        } else if (op == "netcdf.defVar") {
            nargincheck(args, 5, 5);
            std::vector<int> dimids = intVector(args[4]);
            int id = 0;
            checkNc(nc_def_var(asInt(args[1]), asString(args[2]).c_str(), asInt(args[3]),
                static_cast<int>(dimids.size()), dimids.data(), &id));
            out << dscalar(id);
        } else if (op == "netcdf.inqVarID") {
            nargincheck(args, 3, 3);
            out << dscalar(varIdByName(asInt(args[1]), asString(args[2])));
        } else if (op == "netcdf.inqVar") {
            nargincheck(args, 3, 3);
            int ncid = asInt(args[1]);
            int varid = asInt(args[2]);
            char name[NC_MAX_NAME + 1];
            nc_type xtype;
            int ndims = 0, natts = 0, dimids[NC_MAX_VAR_DIMS];
            checkNc(nc_inq_var(ncid, varid, name, &xtype, &ndims, dimids, &natts));
            std::vector<double> dims;
            for (int k = 0; k < ndims; ++k) {
                dims.push_back(dimids[k]);
            }
            out << text(name) << dscalar(xtype) << rowDoubles(dims) << dscalar(natts);
        } else if (op == "netcdf.putVar") {
            nargincheck(args, 4, 7);
            if (args.size() == 5) {
                std::vector<size_t> start = sizeVector(args[3], false);
                std::vector<indexType> raw = args[4].dimensions().asVector();
                std::vector<size_t> count;
                for (indexType v : raw) {
                    count.push_back(static_cast<size_t>(v));
                }
                std::vector<ptrdiff_t> stride(start.size(), 1);
                writeVar(asInt(args[1]), asInt(args[2]), args[4], start, count, stride);
            } else if (args.size() > 5) {
                std::vector<size_t> start = sizeVector(args[3], false);
                std::vector<size_t> count = sizeVector(args[4], false);
                std::vector<ptrdiff_t> stride = args.size() > 6
                    ? strideVector(args[5])
                    : std::vector<ptrdiff_t>(start.size(), 1);
                writeVar(asInt(args[1]), asInt(args[2]), args.back(), start, count, stride);
            } else {
                writeVar(asInt(args[1]), asInt(args[2]), args[3]);
            }
        } else if (op == "netcdf.getVar") {
            nargincheck(args, 3, 6);
            if (args.size() > 3) {
                std::vector<size_t> start = sizeVector(args[3], false);
                std::vector<size_t> count = args.size() > 4
                    ? sizeVector(args[4], false)
                    : shapeOfVar(asInt(args[1]), asInt(args[2]));
                std::vector<ptrdiff_t> stride = args.size() > 5
                    ? strideVector(args[5])
                    : std::vector<ptrdiff_t>(start.size(), 1);
                out << readVar(asInt(args[1]), asInt(args[2]), start, count, stride);
            } else {
                out << readVar(asInt(args[1]), asInt(args[2]));
            }
        } else if (op == "netcdf.renameVar") {
            nargincheck(args, 4, 4);
            checkNc(nc_rename_var(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str()));
        } else if (op == "netcdf.putAtt") {
            nargincheck(args, 5, 5);
            writeAtt(asInt(args[1]), asInt(args[2]), asString(args[3]), args[4]);
        } else if (op == "netcdf.att" || op == "netcdf.getAtt") {
            nargincheck(args, 4, 4);
            out << readAtt(asInt(args[1]), asInt(args[2]), asString(args[3]));
        } else if (op == "netcdf.inqAtt") {
            nargincheck(args, 4, 4);
            nc_type xtype;
            size_t len = 0;
            checkNc(nc_inq_att(
                asInt(args[1]), asInt(args[2]), asString(args[3]).c_str(), &xtype, &len));
            out << dscalar(xtype) << dscalar(static_cast<double>(len));
        } else if (op == "netcdf.inqAttName") {
            nargincheck(args, 4, 4);
            char name[NC_MAX_NAME + 1];
            checkNc(nc_inq_attname(asInt(args[1]), asInt(args[2]), asInt(args[3]), name));
            out << text(name);
        } else if (op == "netcdf.inqAttID") {
            nargincheck(args, 4, 4);
            int id = 0;
            checkNc(nc_inq_attid(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str(), &id));
            out << dscalar(id);
        } else if (op == "netcdf.delAtt") {
            nargincheck(args, 4, 4);
            checkNc(nc_del_att(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str()));
        } else if (op == "netcdf.renameAtt") {
            nargincheck(args, 5, 5);
            checkNc(nc_rename_att(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str(),
                asString(args[4]).c_str()));
        } else if (op == "netcdf.copyAtt") {
            nargincheck(args, 6, 6);
            checkNc(nc_copy_att(asInt(args[1]), asInt(args[2]), asString(args[3]).c_str(),
                asInt(args[4]), asInt(args[5])));
        } else if (op == "netcdf.defGrp") {
            nargincheck(args, 3, 3);
            int grpid = 0;
            checkNc(nc_def_grp(asInt(args[1]), asString(args[2]).c_str(), &grpid));
            out << dscalar(grpid);
        } else if (op == "netcdf.inqNcid") {
            nargincheck(args, 3, 3);
            int grpid = 0;
            checkNc(nc_inq_ncid(asInt(args[1]), asString(args[2]).c_str(), &grpid));
            out << dscalar(grpid);
        } else if (op == "netcdf.inqGrpName" || op == "netcdf.inqGrpNameFull") {
            nargincheck(args, 2, 2);
            char name[NC_MAX_NAME + 1];
            if (op == "netcdf.inqGrpName") {
                checkNc(nc_inq_grpname(asInt(args[1]), name));
            } else {
                size_t len = 0;
                checkNc(nc_inq_grpname_full(asInt(args[1]), &len, nullptr));
                std::vector<char> full(len + 1);
                checkNc(nc_inq_grpname_full(asInt(args[1]), &len, full.data()));
                out << text(full.data());
                return out;
            }
            out << text(name);
        } else if (op == "netcdf.inqGrpParent") {
            nargincheck(args, 2, 2);
            int parent = 0;
            checkNc(nc_inq_grp_parent(asInt(args[1]), &parent));
            out << dscalar(parent);
        } else if (op == "netcdf.inqGrps" || op == "netcdf.inqDimIDs" || op == "netcdf.inqVarIDs"
            || op == "netcdf.inqUnlimDims") {
            nargincheck(args, 2, 2);
            int n = 0;
            if (op == "netcdf.inqGrps") {
                checkNc(nc_inq_grps(asInt(args[1]), &n, nullptr));
            } else if (op == "netcdf.inqDimIDs") {
                checkNc(nc_inq_dimids(asInt(args[1]), &n, nullptr, 0));
            } else if (op == "netcdf.inqVarIDs") {
                checkNc(nc_inq_varids(asInt(args[1]), &n, nullptr));
            } else {
                checkNc(nc_inq_unlimdims(asInt(args[1]), &n, nullptr));
            }
            std::vector<int> ids(n);
            if (op == "netcdf.inqGrps") {
                checkNc(nc_inq_grps(asInt(args[1]), &n, ids.data()));
            } else if (op == "netcdf.inqDimIDs") {
                checkNc(nc_inq_dimids(asInt(args[1]), &n, ids.data(), 0));
            } else if (op == "netcdf.inqVarIDs") {
                checkNc(nc_inq_varids(asInt(args[1]), &n, ids.data()));
            } else {
                checkNc(nc_inq_unlimdims(asInt(args[1]), &n, ids.data()));
            }
            std::vector<double> d;
            for (int id : ids) {
                d.push_back(id);
            }
            out << rowDoubles(d);
        } else if (op == "netcdf.defVarChunking") {
            nargincheck(args, 5, 5);
            std::vector<size_t> chunks = sizeVector(args[4], false);
            checkNc(
                nc_def_var_chunking(asInt(args[1]), asInt(args[2]), asInt(args[3]), chunks.data()));
        } else if (op == "netcdf.inqVarChunking") {
            nargincheck(args, 3, 3);
            int ncid = asInt(args[1]);
            int varid = asInt(args[2]);
            int storage = 0;
            std::vector<size_t> chunks(shapeOfVar(ncid, varid).size());
            checkNc(nc_inq_var_chunking(ncid, varid, &storage, chunks.data()));
            std::vector<double> d;
            for (size_t v : chunks) {
                d.push_back(static_cast<double>(v));
            }
            out << dscalar(storage) << rowDoubles(d);
        } else if (op == "netcdf.defVarDeflate") {
            nargincheck(args, 6, 6);
            checkNc(nc_def_var_deflate(
                asInt(args[1]), asInt(args[2]), asInt(args[3]), asInt(args[4]), asInt(args[5])));
        } else if (op == "netcdf.inqVarDeflate") {
            nargincheck(args, 3, 3);
            int shuffle = 0, deflate = 0, level = 0;
            checkNc(nc_inq_var_deflate(asInt(args[1]), asInt(args[2]), &shuffle, &deflate, &level));
            out << dscalar(shuffle) << dscalar(deflate) << dscalar(level);
        } else if (op == "netcdf.defVarFill") {
            nargincheck(args, 5, 5);
            checkNc(nc_def_var_fill(
                asInt(args[1]), asInt(args[2]), asInt(args[3]), args[4].dataPointer()));
        } else if (op == "netcdf.inqVarFill") {
            nargincheck(args, 3, 3);
            char name[NC_MAX_NAME + 1];
            nc_type xtype;
            int ndims = 0, natts = 0, dimids[NC_MAX_VAR_DIMS], noFill = 0;
            checkNc(
                nc_inq_var(asInt(args[1]), asInt(args[2]), name, &xtype, &ndims, dimids, &natts));
            ArrayOf fillValue
                = NetcdfGateway::netcdfReadFill(asInt(args[1]), asInt(args[2]), xtype, noFill);
            out << dscalar(noFill) << fillValue;
        } else if (op == "netcdf.defVarFletcher32") {
            nargincheck(args, 4, 4);
            checkNc(nc_def_var_fletcher32(asInt(args[1]), asInt(args[2]), asInt(args[3])));
        } else if (op == "netcdf.inqVarFletcher32") {
            nargincheck(args, 3, 3);
            int fletcher = 0;
            checkNc(nc_inq_var_fletcher32(asInt(args[1]), asInt(args[2]), &fletcher));
            out << dscalar(fletcher);
        } else if (op == "netcdf.defVlen") {
            nargincheck(args, 4, 4);
            nc_type xtype = 0;
            checkNc(nc_def_vlen(asInt(args[1]), asString(args[2]).c_str(), asInt(args[3]), &xtype));
            out << dscalar(xtype);
        } else if (op == "netcdf.inqUserType") {
            nargincheck(args, 3, 3);
            char name[NC_MAX_NAME + 1];
            size_t size = 0;
            nc_type base = 0;
            size_t nfields = 0;
            int classp = 0;
            checkNc(nc_inq_user_type(
                asInt(args[1]), asInt(args[2]), name, &size, &base, &nfields, &classp));
            out << text(name) << dscalar(static_cast<double>(size)) << dscalar(base)
                << dscalar(static_cast<double>(nfields)) << dscalar(classp);
        } else if (op == "netcdf.inqVlen") {
            nargincheck(args, 3, 3);
            char name[NC_MAX_NAME + 1];
            size_t datumSize = 0;
            nc_type base = 0;
            checkNc(nc_inq_vlen(asInt(args[1]), asInt(args[2]), name, &datumSize, &base));
            out << text(name) << dscalar(static_cast<double>(datumSize)) << dscalar(base);
        } else if (op == "netcdf.setDefaultFormat") {
            nargincheck(args, 2, 2);
            int old = 0;
            checkNc(nc_set_default_format(asInt(args[1]), &old));
            out << dscalar(old);
        } else if (op == "netcdf.getChunkCache") {
            nargincheck(args, 1, 1);
            size_t size = 0, nelems = 0;
            float preemption = 0;
            checkNc(nc_get_chunk_cache(&size, &nelems, &preemption));
            out << dscalar(static_cast<double>(size)) << dscalar(static_cast<double>(nelems))
                << dscalar(preemption);
        } else if (op == "netcdf.setChunkCache") {
            nargincheck(args, 4, 4);
            checkNc(nc_set_chunk_cache(
                asSize(args[1]), asSize(args[2]), static_cast<float>(args[3].asDoubleScalar())));
        } else {
            Error(L"Nelson:netcdf:unsupportedOperation", L"Unsupported netCDF operation.");
        }
        return out;
    }
    //=============================================================================
} // namespace
//=============================================================================
} // namespace Nelson
//=============================================================================
