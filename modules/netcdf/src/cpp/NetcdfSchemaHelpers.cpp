//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NetcdfSchemaHelpers.hpp"
#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <netcdf.h>
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NetcdfTypes.hpp"
//=============================================================================
namespace Nelson::NetcdfGateway {
//=============================================================================
static void
checkNc(int status);
static std::string
asString(const ArrayOf& value);
static int
asInt(const ArrayOf& value);
static size_t
asSize(const ArrayOf& value);
static ArrayOf
dscalar(double v);
static ArrayOf
lscalar(bool v);
static ArrayOf
text(const std::string& v);
static ArrayOf
rowDoubles(const std::vector<double>& values);
static ArrayOf
cellArray(const std::vector<ArrayOf>& values);
static ArrayOf
structArray(const stringVector& names, const std::vector<ArrayOfVector>& rows);
static ArrayOf
structFieldAt(const ArrayOf& value, indexType index, const std::string& field);
static std::vector<size_t>
sizeVector(const ArrayOf& value);
static void
writeAtt(int ncid, int varid, const std::string& name, const ArrayOf& value);
static void
writeFill(int ncid, int varid, nc_type xtype, bool noFill, const ArrayOf& value);
static std::vector<int>
visibleUnlimIds(int ncid);
static ArrayOf
variableInfo(int ncid, const std::vector<int>& unlimIds);
static void
writeSchemaElementInto(
    int ncid, const ArrayOf& schema, indexType index, const std::map<int, int>& parentDimMap);
//=============================================================================
ArrayOf
netcdfGroupInfo(int ncid)
{
    int nGroups = 0;
    checkNc(nc_inq_grps(ncid, &nGroups, nullptr));
    std::vector<int> groupIds(static_cast<size_t>(nGroups));
    if (nGroups > 0) {
        checkNc(nc_inq_grps(ncid, &nGroups, groupIds.data()));
    }
    std::vector<ArrayOfVector> groupRows;
    for (int groupId : groupIds) {
        char groupName[NC_MAX_NAME + 1];
        checkNc(nc_inq_grpname(groupId, groupName));
        std::vector<int> unlimIds = visibleUnlimIds(groupId);
        int nDims = 0;
        checkNc(nc_inq_dimids(groupId, &nDims, nullptr, 0));
        std::vector<int> dimIds(static_cast<size_t>(nDims));
        if (nDims > 0) {
            checkNc(nc_inq_dimids(groupId, &nDims, dimIds.data(), 0));
        }
        std::vector<ArrayOfVector> dimRows;
        for (int dimId : dimIds) {
            char dimName[NC_MAX_NAME + 1];
            size_t dimLen = 0;
            checkNc(nc_inq_dim(groupId, dimId, dimName, &dimLen));
            bool unlimited = std::find(unlimIds.begin(), unlimIds.end(), dimId) != unlimIds.end();
            ArrayOfVector row;
            row << text(dimName) << dscalar(static_cast<double>(dimLen)) << lscalar(unlimited)
                << dscalar(dimId);
            dimRows.push_back(row);
        }
        int nAtts = 0;
        checkNc(nc_inq_natts(groupId, &nAtts));
        std::vector<ArrayOfVector> attRows;
        for (int k = 0; k < nAtts; ++k) {
            char attName[NC_MAX_NAME + 1];
            checkNc(nc_inq_attname(groupId, NC_GLOBAL, k, attName));
            ArrayOfVector row;
            row << text(attName) << netcdfReadAtt(groupId, NC_GLOBAL, attName);
            attRows.push_back(row);
        }
        ArrayOfVector row;
        row << text(groupName) << structArray({ "Name", "Length", "Unlimited", "ID" }, dimRows)
            << variableInfo(groupId, unlimIds) << structArray({ "Name", "Value" }, attRows)
            << netcdfGroupInfo(groupId);
        groupRows.push_back(row);
    }
    return structArray({ "Name", "Dimensions", "Variables", "Attributes", "Groups" }, groupRows);
}
//=============================================================================
void
netcdfWriteSchemaInto(int ncid, const ArrayOf& schema)
{
    writeSchemaElementInto(ncid, schema, 0, std::map<int, int>());
}
//=============================================================================
static std::vector<int>
visibleUnlimIds(int ncid)
{
    std::vector<int> ids;
    int current = ncid;
    while (true) {
        int nUnlim = 0;
        if (nc_inq_unlimdims(current, &nUnlim, nullptr) == NC_NOERR && nUnlim > 0) {
            std::vector<int> currentIds(static_cast<size_t>(nUnlim));
            if (nc_inq_unlimdims(current, &nUnlim, currentIds.data()) == NC_NOERR) {
                for (int id : currentIds) {
                    if (std::find(ids.begin(), ids.end(), id) == ids.end()) {
                        ids.push_back(id);
                    }
                }
            }
        }
        int parent = 0;
        if (nc_inq_grp_parent(current, &parent) != NC_NOERR) {
            break;
        }
        current = parent;
    }
    return ids;
}
//=============================================================================
static ArrayOf
variableInfo(int ncid, const std::vector<int>& unlimIds)
{
    int nVars = 0;
    checkNc(nc_inq_varids(ncid, &nVars, nullptr));
    std::vector<int> varIds(static_cast<size_t>(nVars));
    if (nVars > 0) {
        checkNc(nc_inq_varids(ncid, &nVars, varIds.data()));
    }
    std::vector<ArrayOfVector> varRows;
    for (int varId : varIds) {
        char name[NC_MAX_NAME + 1];
        nc_type xtype;
        int nd = 0, natts = 0, ids[NC_MAX_VAR_DIMS];
        checkNc(nc_inq_var(ncid, varId, name, &xtype, &nd, ids, &natts));
        std::vector<double> dims;
        std::vector<double> sizeValues;
        std::vector<ArrayOfVector> varDimRows;
        for (int j = 0; j < nd; ++j) {
            char dimName[NC_MAX_NAME + 1];
            size_t dimLen = 0;
            checkNc(nc_inq_dim(ncid, ids[j], dimName, &dimLen));
            dims.push_back(static_cast<double>(ids[j]));
            sizeValues.push_back(static_cast<double>(dimLen));
            bool unlimited = std::find(unlimIds.begin(), unlimIds.end(), ids[j]) != unlimIds.end();
            ArrayOfVector dimRow;
            dimRow << text(dimName) << dscalar(static_cast<double>(dimLen)) << lscalar(unlimited)
                   << dscalar(ids[j]);
            varDimRows.push_back(dimRow);
        }
        std::vector<ArrayOfVector> varAttRows;
        for (int j = 0; j < natts; ++j) {
            char attName[NC_MAX_NAME + 1];
            checkNc(nc_inq_attname(ncid, varId, j, attName));
            if (std::string(attName) == "_FillValue") {
                continue;
            }
            ArrayOfVector attRow;
            attRow << text(attName) << netcdfReadAtt(ncid, varId, attName);
            varAttRows.push_back(attRow);
        }
        int storage = 0;
        std::vector<double> chunkValues;
        std::vector<size_t> chunks(static_cast<size_t>(nd));
        if (nd > 0 && nc_inq_var_chunking(ncid, varId, &storage, chunks.data()) == NC_NOERR
            && storage == NC_CHUNKED) {
            for (size_t value : chunks) {
                chunkValues.push_back(static_cast<double>(value));
            }
        }
        int shuffle = 0, deflate = 0, deflateLevel = 0, fletcher32 = 0, noFill = 0;
        nc_inq_var_deflate(ncid, varId, &shuffle, &deflate, &deflateLevel);
        nc_inq_var_fletcher32(ncid, varId, &fletcher32);
        ArrayOf fillValue = netcdfReadFill(ncid, varId, xtype, noFill);
        ArrayOfVector row;
        row << text(name) << dscalar(static_cast<double>(xtype)) << text(netcdfTypeName(xtype))
            << rowDoubles(dims) << dscalar(static_cast<double>(natts)) << dscalar(varId)
            << rowDoubles(sizeValues)
            << structArray({ "Name", "Length", "Unlimited", "ID" }, varDimRows)
            << structArray({ "Name", "Value" }, varAttRows) << rowDoubles(chunkValues)
            << dscalar(storage) << dscalar(deflate ? deflateLevel : 0) << lscalar(shuffle != 0)
            << lscalar(fletcher32 != 0) << fillValue << lscalar(noFill != 0);
        varRows.push_back(row);
    }
    return structArray({ "Name", "DatatypeID", "Datatype", "DimensionIDs", "NumAttributes", "ID",
                           "Size", "Dimensions", "Attributes", "ChunkSize", "Storage",
                           "DeflateLevel", "Shuffle", "Fletcher32", "FillValue", "NoFill" },
        varRows);
}
//=============================================================================
static void
writeSchemaElementInto(
    int ncid, const ArrayOf& schema, indexType index, const std::map<int, int>& parentDimMap)
{
    std::map<int, int> dimMap = parentDimMap;
    if (schema.isField("Dimensions")) {
        ArrayOf dims = structFieldAt(schema, index, "Dimensions");
        for (indexType k = 0; k < dims.elementCount(); ++k) {
            std::string name = asString(structFieldAt(dims, k, "Name"));
            size_t len = asSize(structFieldAt(dims, k, "Length"));
            bool unlimited = structFieldAt(dims, k, "Unlimited").asLogicalScalar(true);
            int oldId = asInt(structFieldAt(dims, k, "ID"));
            int dimid = 0;
            checkNc(nc_def_dim(ncid, name.c_str(), unlimited ? NC_UNLIMITED : len, &dimid));
            dimMap[oldId] = dimid;
        }
    }
    if (schema.isField("Attributes")) {
        ArrayOf atts = structFieldAt(schema, index, "Attributes");
        for (indexType k = 0; k < atts.elementCount(); ++k) {
            writeAtt(ncid, NC_GLOBAL, asString(structFieldAt(atts, k, "Name")),
                structFieldAt(atts, k, "Value"));
        }
    }
    if (schema.isField("Variables")) {
        ArrayOf vars = structFieldAt(schema, index, "Variables");
        for (indexType k = 0; k < vars.elementCount(); ++k) {
            std::vector<double> raw = structFieldAt(vars, k, "DimensionIDs").asDoubleVector();
            std::vector<int> dimids;
            for (double idValue : raw) {
                int oldId = static_cast<int>(idValue);
                auto it = dimMap.find(oldId);
                if (it == dimMap.end()) {
                    Error(L"Nelson:netcdf:undefinedDimension",
                        L"Schema variable references an undefined dimension.");
                }
                dimids.push_back(it->second);
            }
            int varid = 0;
            nc_type xtype = static_cast<nc_type>(asInt(structFieldAt(vars, k, "DatatypeID")));
            checkNc(nc_def_var(ncid, asString(structFieldAt(vars, k, "Name")).c_str(), xtype,
                static_cast<int>(dimids.size()), dimids.empty() ? nullptr : dimids.data(), &varid));
            if (vars.isField("ChunkSize")) {
                std::vector<size_t> chunks = sizeVector(structFieldAt(vars, k, "ChunkSize"));
                int storage = vars.isField("Storage")
                    ? asInt(structFieldAt(vars, k, "Storage"))
                    : (chunks.empty() ? NC_CONTIGUOUS : NC_CHUNKED);
                if (storage == NC_CONTIGUOUS) {
                    checkNc(nc_def_var_chunking(ncid, varid, NC_CONTIGUOUS, nullptr));
                } else if (!chunks.empty()) {
                    checkNc(nc_def_var_chunking(ncid, varid, NC_CHUNKED, chunks.data()));
                }
            }
            if (vars.isField("DeflateLevel")) {
                int level = asInt(structFieldAt(vars, k, "DeflateLevel"));
                int shuffle = vars.isField("Shuffle")
                    && structFieldAt(vars, k, "Shuffle").asLogicalScalar(true);
                if (level > 0 || shuffle != 0) {
                    checkNc(nc_def_var_deflate(ncid, varid, shuffle, level > 0, level));
                }
            }
            if (vars.isField("Fletcher32")
                && structFieldAt(vars, k, "Fletcher32").asLogicalScalar(true)) {
                checkNc(nc_def_var_fletcher32(ncid, varid, 1));
            }
            if (vars.isField("NoFill") && vars.isField("FillValue")) {
                bool noFill = structFieldAt(vars, k, "NoFill").asLogicalScalar(true);
                writeFill(ncid, varid, xtype, noFill, structFieldAt(vars, k, "FillValue"));
            }
            if (vars.isField("Attributes")) {
                ArrayOf atts = structFieldAt(vars, k, "Attributes");
                for (indexType j = 0; j < atts.elementCount(); ++j) {
                    writeAtt(ncid, varid, asString(structFieldAt(atts, j, "Name")),
                        structFieldAt(atts, j, "Value"));
                }
            }
        }
    }
    if (schema.isField("Groups")) {
        ArrayOf groups = structFieldAt(schema, index, "Groups");
        for (indexType k = 0; k < groups.elementCount(); ++k) {
            int grpid = 0;
            checkNc(nc_def_grp(ncid, asString(structFieldAt(groups, k, "Name")).c_str(), &grpid));
            writeSchemaElementInto(grpid, groups, k, dimMap);
        }
    }
}
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
        Error(L"Nelson:netcdf:mustBeNonnegativeInteger", L"Expected a non-negative integer value.");
    }
    return static_cast<size_t>(v);
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
    return ArrayOf::characterArray(utf8_to_wstring(v));
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
cellArray(const std::vector<ArrayOf>& values)
{
    Dimensions dims(1, static_cast<indexType>(values.size()));
    ArrayOf* data = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size()));
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
static std::vector<size_t>
sizeVector(const ArrayOf& value)
{
    std::vector<double> raw = value.asDoubleVector();
    std::vector<size_t> out;
    for (double v : raw) {
        out.push_back(static_cast<size_t>(v));
    }
    return out;
}
//=============================================================================
ArrayOf
netcdfReadAtt(int ncid, int varid, const std::string& name)
{
    nc_type xtype;
    size_t len = 0;
    checkNc(nc_inq_att(ncid, varid, name.c_str(), &xtype, &len));
    if (xtype == NC_CHAR) {
        std::vector<char> s(len + 1, '\0');
        checkNc(nc_get_att_text(ncid, varid, name.c_str(), s.data()));
        return text(std::string(s.data(), len));
    }
    Dimensions dims(1, static_cast<indexType>(len));
    if (xtype == NC_FLOAT) {
        single* data = static_cast<single*>(ArrayOf::allocateArrayOf(NLS_SINGLE, len));
        checkNc(nc_get_att_float(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_SINGLE, dims, data);
    }
    if (xtype == NC_BYTE) {
        int8* data = static_cast<int8*>(ArrayOf::allocateArrayOf(NLS_INT8, len));
        checkNc(nc_get_att_schar(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_INT8, dims, data);
    }
    if (xtype == NC_UBYTE) {
        uint8* data = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, len));
        checkNc(nc_get_att_uchar(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_UINT8, dims, data);
    }
    if (xtype == NC_SHORT) {
        int16* data = static_cast<int16*>(ArrayOf::allocateArrayOf(NLS_INT16, len));
        checkNc(nc_get_att_short(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_INT16, dims, data);
    }
    if (xtype == NC_USHORT) {
        uint16* data = static_cast<uint16*>(ArrayOf::allocateArrayOf(NLS_UINT16, len));
        checkNc(nc_get_att_ushort(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_UINT16, dims, data);
    }
    if (xtype == NC_INT) {
        int32* data = static_cast<int32*>(ArrayOf::allocateArrayOf(NLS_INT32, len));
        checkNc(nc_get_att_int(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_INT32, dims, data);
    }
    if (xtype == NC_UINT) {
        uint32* data = static_cast<uint32*>(ArrayOf::allocateArrayOf(NLS_UINT32, len));
        checkNc(nc_get_att_uint(ncid, varid, name.c_str(), data));
        return ArrayOf(NLS_UINT32, dims, data);
    }
    if (xtype == NC_INT64) {
        int64* data = static_cast<int64*>(ArrayOf::allocateArrayOf(NLS_INT64, len));
        checkNc(nc_get_att_longlong(ncid, varid, name.c_str(), reinterpret_cast<long long*>(data)));
        return ArrayOf(NLS_INT64, dims, data);
    }
    if (xtype == NC_UINT64) {
        uint64* data = static_cast<uint64*>(ArrayOf::allocateArrayOf(NLS_UINT64, len));
        checkNc(nc_get_att_ulonglong(
            ncid, varid, name.c_str(), reinterpret_cast<unsigned long long*>(data)));
        return ArrayOf(NLS_UINT64, dims, data);
    }
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, len));
    checkNc(nc_get_att_double(ncid, varid, name.c_str(), data));
    return ArrayOf(NLS_DOUBLE, dims, data);
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
    checkNc(nc_put_att(ncid, varid, name.c_str(), netcdfTypeFromArray(value), value.elementCount(),
        value.dataPointer()));
}
//=============================================================================
ArrayOf
netcdfReadFill(int ncid, int varid, nc_type xtype, int& noFill)
{
    switch (xtype) {
    case NC_FLOAT: {
        single value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_BYTE: {
        int8 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_UBYTE: {
        uint8 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_SHORT: {
        int16 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_USHORT: {
        uint16 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_INT: {
        int32 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_UINT: {
        uint32 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_INT64: {
        int64 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_UINT64: {
        uint64 value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(static_cast<double>(value));
    }
    case NC_CHAR: {
        char value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return text(std::string(1, value));
    }
    default: {
        double value = 0;
        checkNc(nc_inq_var_fill(ncid, varid, &noFill, &value));
        return dscalar(value);
    }
    }
}
//=============================================================================
static void
writeFill(int ncid, int varid, nc_type xtype, bool noFill, const ArrayOf& value)
{
    int mode = noFill ? NC_NOFILL : NC_FILL;
    if (noFill) {
        checkNc(nc_def_var_fill(ncid, varid, mode, nullptr));
        return;
    }
    if (xtype == NC_CHAR) {
        std::string s = asString(value);
        char fillValue = s.empty() ? '\0' : s[0];
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        return;
    }
    double scalar = value.asDoubleScalar();
    switch (xtype) {
    case NC_FLOAT: {
        single fillValue = static_cast<single>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_BYTE: {
        signed char fillValue = static_cast<signed char>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_UBYTE: {
        unsigned char fillValue = static_cast<unsigned char>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_SHORT: {
        short fillValue = static_cast<short>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_USHORT: {
        unsigned short fillValue = static_cast<unsigned short>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_INT: {
        int fillValue = static_cast<int>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_UINT: {
        unsigned int fillValue = static_cast<unsigned int>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_INT64: {
        long long fillValue = static_cast<long long>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    case NC_UINT64: {
        unsigned long long fillValue = static_cast<unsigned long long>(scalar);
        checkNc(nc_def_var_fill(ncid, varid, mode, &fillValue));
        break;
    }
    default:
        checkNc(nc_def_var_fill(ncid, varid, mode, &scalar));
        break;
    }
}
//=============================================================================
} // namespace Nelson::NetcdfGateway
//=============================================================================
