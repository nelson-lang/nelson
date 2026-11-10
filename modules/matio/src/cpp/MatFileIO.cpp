//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include <cstring>
#include <vector>
#include "MatFileIO.hpp"
#include "PartialFileAccess.hpp"
#ifdef NLS_MATFILE_WITH_HDF5
#include "h5Hyperslab.hpp"
#endif
#include "LoadMatioVariable.hpp"
#include "SaveMatioVariable.hpp"
#include "matioHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
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
    // Nelson class of a matio variable, NLS_UNKNOWN for objects and other classes
    NelsonType
    variableType(const matvar_t* matvar)
    {
        const bool isComplex = matvar->isComplex != 0;
        switch (matvar->class_type) {
        case MAT_C_DOUBLE:
            return isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
        case MAT_C_SINGLE:
            return isComplex ? NLS_SCOMPLEX : NLS_SINGLE;
        case MAT_C_INT8:
            return NLS_INT8;
        case MAT_C_UINT8:
            return matvar->isLogical != 0 ? NLS_LOGICAL : NLS_UINT8;
        case MAT_C_INT16:
            return NLS_INT16;
        case MAT_C_UINT16:
            return NLS_UINT16;
        case MAT_C_INT32:
            return NLS_INT32;
        case MAT_C_UINT32:
            return NLS_UINT32;
        case MAT_C_INT64:
            return NLS_INT64;
        case MAT_C_UINT64:
            return NLS_UINT64;
        case MAT_C_CHAR:
            return NLS_CHAR;
        case MAT_C_CELL:
            return NLS_CELL_ARRAY;
        case MAT_C_STRUCT:
            return NLS_STRUCT_ARRAY;
        case MAT_C_SPARSE:
            if (matvar->isLogical != 0) {
                return NLS_LOGICAL;
            }
            return isComplex ? NLS_DCOMPLEX : NLS_DOUBLE;
        default:
            return NLS_UNKNOWN;
        }
    }
    //=============================================================================
    class MatioPartialFile : public PartialFileBackend
    {
    public:
        MatioPartialFile(Evaluator* eval, const std::wstring& filename)
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
            mat_t* mat = openMatFile(MAT_ACC_RDONLY);
            isV73 = Mat_GetVersion(mat) == MAT_FT_MAT73;
            matvar_t* matvar = Mat_VarReadInfo(mat, name.c_str());
            if (matvar != nullptr) {
                variable.exists = true;
                variable.type = variableType(matvar);
                variable.isSparse = matvar->class_type == MAT_C_SPARSE;
                variable.dims = getMatVarDimensions(matvar);
                Mat_VarFree(matvar);
            }
            Mat_Close(mat);
            return variable;
        }
        //=============================================================================
        ArrayOf
        readWhole(const std::string& name) override
        {
            mat_t* mat = openMatFile(MAT_ACC_RDONLY);
            matvar_t* matvar = Mat_VarRead(mat, name.c_str());
            if (matvar == nullptr) {
                Mat_Close(mat);
                Error("Nelson:matio:cannotReadVariable", "Cannot read variable: %s", name);
            }
            ArrayOf value;
            bool ok = LoadMatioVariable(eval, matvar, false, value);
            Mat_VarFree(matvar);
            Mat_Close(mat);
            if (!ok) {
                Error("Nelson:matio:cannotReadVariable", "Cannot read variable: %s", name);
            }
            return value;
        }
        //=============================================================================
        void
        writeWhole(const std::string& name, const ArrayOf& value) override
        {
            mat_t* mat = nullptr;
            if (fileExists(filename)) {
                mat = openMatFile(MAT_ACC_RDWR);
            } else {
                mat = Mat_CreateVer(
                    utf8filename.c_str(), createHeaderMatioFile().c_str(), MAT_FT_MAT73);
                if (mat == nullptr) {
                    Error(L"Nelson:matio:cannotSaveFile", L"Cannot save file.");
                }
            }
            const mat_ft version = Mat_GetVersion(mat);
            matvar_t* matvar = SaveMatioVariable(eval, name, value, version);
            if (matvar == nullptr) {
                Mat_Close(mat);
                Error("Nelson:matio:cannotSaveVariable", "Cannot save variable: %s", name);
            }
            matvar_t* previous = Mat_VarReadInfo(mat, name.c_str());
            if (previous != nullptr) {
                Mat_VarFree(previous);
                mat = deleteVariable(mat, name, version);
            }
            const matio_compression compression
                = version == MAT_FT_MAT4 ? MAT_COMPRESSION_NONE : MAT_COMPRESSION_ZLIB;
            const int err = Mat_VarWrite(mat, matvar, compression);
            Mat_VarFree(matvar);
            Mat_Close(mat);
            if (err != 0) {
                Error("Nelson:matio:cannotSaveVariable", "Cannot save variable: %s", name);
            }
        }
        //=============================================================================
        // v7.3 numeric variable: matio reads the strided block from the file
        bool
        readBlock(const std::string& name, const PartialFileVariable& variable,
            const PartialFileBlock& block, ArrayOf& result) override
        {
            if (!isV73 || variable.isSparse || variable.type == NLS_CHAR
                || variable.type == NLS_CELL_ARRAY || variable.type == NLS_STRUCT_ARRAY) {
                return false;
            }
            const size_t rank = variable.dims.length();
            std::vector<int> start(rank), stride(rank), edge(rank);
            for (size_t k = 0; k < rank; k++) {
                start[k] = static_cast<int>(block.start[k] - 1);
                stride[k] = static_cast<int>(block.stride[k]);
                edge[k] = static_cast<int>(block.count[k]);
            }
            const NelsonType type = variable.type;
            const indexType count = block.elementCount();
            mat_t* mat = openMatFile(MAT_ACC_RDONLY);
            matvar_t* matvar = Mat_VarReadInfo(mat, name.c_str());
            if (matvar == nullptr) {
                Mat_Close(mat);
                return false;
            }
            void* data = ArrayOf::allocateArrayOf(type, count, stringVector(), false);
            // the array owns the buffer, also when the read fails
            ArrayOf values(type, block.resultDimensions(), data);
            int err = 0;
            if (type == NLS_DCOMPLEX || type == NLS_SCOMPLEX) {
                const size_t partSize = (type == NLS_DCOMPLEX) ? sizeof(double) : sizeof(single);
                std::vector<char> re(partSize * count), im(partSize * count);
                mat_complex_split_t split { re.data(), im.data() };
                err = Mat_VarReadData(
                    mat, matvar, &split, start.data(), stride.data(), edge.data());
                auto* out = static_cast<char*>(data);
                for (indexType k = 0; err == 0 && k < count; k++) {
                    std::memcpy(out + (2 * k) * partSize, re.data() + k * partSize, partSize);
                    std::memcpy(out + (2 * k + 1) * partSize, im.data() + k * partSize, partSize);
                }
            } else {
                err = Mat_VarReadData(mat, matvar, data, start.data(), stride.data(), edge.data());
            }
            Mat_VarFree(matvar);
            Mat_Close(mat);
            if (err != 0) {
                return false;
            }
            result = values;
            return true;
        }
        //=============================================================================
        // matio cannot write a block of a v7.3 file: HDF5 writes it in place
        bool
        writeBlock(const std::string& name, const PartialFileVariable& variable,
            const PartialFileBlock& block, const ArrayOf& data) override
        {
#ifdef NLS_MATFILE_WITH_HDF5
            return isV73
                && h5HyperslabWrite(utf8filename, "/" + name, block, variable.dims.length(), data);
#else
            return false;
#endif
        }
        //=============================================================================
        void
        beforeWholeAccess(const std::string& name) override
        {
            if (isV73) {
                return;
            }
            RaiseWarning("Nelson:matfile:wholeVariableLoad",
                formatI18nMessage(resolveMessageTemplate("Nelson:matfile:wholeVariableLoad",
                                      "'%s' is not a version 7.3 MAT-file: variable '%s' is "
                                      "loaded whole. Save with -v7.3 for partial access."),
                    utf8filename, name));
        }
        //=============================================================================
    private:
        Evaluator* eval;
        std::wstring filename;
        std::string utf8filename;
        bool isV73 = false;
        //=============================================================================
        mat_t*
        openMatFile(int mode) const
        {
            mat_t* mat = Mat_Open(utf8filename.c_str(), mode);
            if (mat == nullptr) {
                Error("Nelson:matfile:notMatFile", "'%s' is not a valid MAT-file.", utf8filename);
            }
            return mat;
        }
        //=============================================================================
        // matio deletes by copying every other variable to a new file: v7.3 unlinks instead
        mat_t*
        deleteVariable(mat_t* mat, const std::string& name, mat_ft version) const
        {
#ifdef NLS_MATFILE_WITH_HDF5
            if (version == MAT_FT_MAT73) {
                Mat_Close(mat);
                const bool unlinked = h5UnlinkVariable(utf8filename, "/" + name);
                mat = openMatFile(MAT_ACC_RDWR);
                if (unlinked) {
                    return mat;
                }
            }
#endif
            Mat_VarDelete(mat, name.c_str());
            return mat;
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
MatFileVariableSize(const std::wstring& filename, const std::string& variableName)
{
    MatioPartialFile file(nullptr, filename);
    return PartialFileVariableSize(file, variableName);
}
//=============================================================================
ArrayOf
MatFileRead(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices)
{
    MatioPartialFile file(eval, filename);
    return PartialFileRead(file, variableName, indices);
}
//=============================================================================
void
MatFileWrite(Evaluator* eval, const std::wstring& filename, const std::string& variableName,
    const ArrayOfVector* indices, const ArrayOf& value)
{
    MatioPartialFile file(eval, filename);
    PartialFileWrite(file, variableName, indices, value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
