//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Nh5FileIO.hpp"
#include "h5Hyperslab.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
#include "h5SaveVariable.hpp"
#include "PartialFileAccess.hpp"
#include "FileSystemWrapper.hpp"
#include "IsValidVariableName.hpp"
#include "StringToClass.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    fileExists(const std::wstring& filename)
    {
        bool permissionDenied = false;
        return FileSystemWrapper::Path::is_regular_file(
            FileSystemWrapper::Path(filename), permissionDenied);
    }
    //=============================================================================
    // Nelson class of a stored variable, NLS_UNKNOWN for objects and other classes
    NelsonType
    storedType(const std::string& className, bool isObject, bool isComplex)
    {
        if (isObject) {
            return NLS_UNKNOWN;
        }
        bool haveError = false;
        NelsonType type = StringToClass(className, haveError);
        if (haveError) {
            return NLS_UNKNOWN;
        }
        switch (type) {
        case NLS_DOUBLE:
            return isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
        case NLS_SINGLE:
            return isComplex ? NLS_SCOMPLEX : NLS_SINGLE;
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_INT64:
        case NLS_UINT64:
        case NLS_LOGICAL:
        case NLS_CHAR:
        case NLS_CELL_ARRAY:
        case NLS_STRUCT_ARRAY:
            return type;
        default:
            return NLS_UNKNOWN;
        }
    }
    //=============================================================================
    class Nh5PartialFile : public PartialFileBackend
    {
    public:
        Nh5PartialFile(Evaluator* eval, const std::wstring& filename)
            : eval(eval), filename(filename), utf8filename(wstring_to_utf8(filename))
        {
        }
        //=============================================================================
        std::string
        fileText() const override
        {
            return utf8filename;
        }
        //=============================================================================
        PartialFileVariable
        describe(const std::string& name) override
        {
            PartialFileVariable variable;
            variable.fileExists = fileExists(filename);
            if (!variable.fileExists) {
                return variable;
            }
            hid_t fid = openNh5File(H5F_ACC_RDONLY);
            variable.exists = hasVariable(fid, name);
            if (variable.exists) {
                const std::string location = "/";
                const bool isComplex = isNelsonComplex(fid, location, name);
                variable.type = storedType(getNelsonClass(fid, location, name),
                    isNelsonObject(fid, location, name), isComplex);
                variable.isSparse = isNelsonSparse(fid, location, name);
                variable.dims = getNelsonDimensions(fid, location, name);
                isEmpty = isNelsonEmpty(fid, location, name);
            }
            H5Fclose(fid);
            return variable;
        }
        //=============================================================================
        ArrayOf
        readWhole(const std::string& name) override
        {
            hid_t fid = openNh5File(H5F_ACC_RDONLY);
            ArrayOf value;
            const bool ok = h5LoadVariable(eval, fid, "/", name, value);
            H5Fclose(fid);
            if (!ok) {
                Error("Nelson:nh5file:cannotReadVariable", "Cannot read variable: %s", name);
            }
            return value;
        }
        //=============================================================================
        void
        writeWhole(const std::string& name, const ArrayOf& value) override
        {
            if (!IsValidVariableName(name)) {
                Error("Nelson:h5save:invalidVariableName", "Invalid variable name: %s", name);
            }
            hid_t fid = H5I_INVALID_HID;
            if (fileExists(filename)) {
                fid = openNh5File(H5F_ACC_RDWR);
                if (getNelsonH5Schema(fid) != NELSON_SCHEMA) {
                    H5Fclose(fid);
                    Error("Nelson:h5save:invalidFileVersion", "Invalid file version.");
                }
            } else {
                fid = createNh5FileWithHeader(filename);
                if (fid == H5I_INVALID_HID) {
                    Error("Nelson:hdf5:openFailed", "Open file failed.");
                }
                updateNelsonH5Header(fid);
            }
            const bool ok = h5SaveVariable(eval, fid, "/", name, value, true);
            H5Fclose(fid);
            if (!ok) {
                Error("Nelson:h5save:cannotSaveVariable", "Cannot save variable: %s", name);
            }
        }
        //=============================================================================
        bool
        readBlock(const std::string& name, const PartialFileVariable& variable,
            const PartialFileBlock& block, ArrayOf& result) override
        {
            return !isEmpty
                && h5HyperslabRead(
                    utf8filename, "/" + name, block, variable.dims.length(), variable.type, result);
        }
        //=============================================================================
        bool
        writeBlock(const std::string& name, const PartialFileVariable& variable,
            const PartialFileBlock& block, const ArrayOf& data) override
        {
            return !isEmpty
                && h5HyperslabWrite(utf8filename, "/" + name, block, variable.dims.length(), data);
        }
        //=============================================================================
    private:
        Evaluator* eval;
        std::wstring filename;
        std::string utf8filename;
        // empty variables hold a one value placeholder dataset
        bool isEmpty = false;
        //=============================================================================
        hid_t
        openNh5File(unsigned int mode) const
        {
            hid_t fid = H5I_INVALID_HID;
            H5E_BEGIN_TRY { fid = H5Fopen(utf8filename.c_str(), mode, H5P_DEFAULT); }
            H5E_END_TRY
            if (fid < 0 || !isNelsonH5File(fid)) {
                if (fid >= 0) {
                    H5Fclose(fid);
                }
                Error("Nelson:nh5file:notNh5File", "'%s' is not a valid .nh5 file.", utf8filename);
            }
            return fid;
        }
        //=============================================================================
        static bool
        hasVariable(hid_t fid, const std::string& name)
        {
            if (!IsValidVariableName(name)) {
                return false;
            }
            htri_t exists = 0;
            H5E_BEGIN_TRY { exists = H5Lexists(fid, ("/" + name).c_str(), H5P_DEFAULT); }
            H5E_END_TRY
            return exists > 0;
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
Nh5FileVariableSize(const std::wstring& filename, const std::string& variableName)
{
    Nh5PartialFile file(nullptr, filename);
    return PartialFileVariableSize(file, variableName);
}
//=============================================================================
ArrayOf
Nh5FileRead(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices)
{
    Nh5PartialFile file(eval, filename);
    return PartialFileRead(file, variableName, indices);
}
//=============================================================================
void
Nh5FileWrite(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices, const ArrayOf& value)
{
    Nh5PartialFile file(eval, filename);
    PartialFileWrite(file, variableName, indices, value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
