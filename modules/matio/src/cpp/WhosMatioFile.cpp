//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <matio.h>
#include <iomanip>
#include <vector>
#include "WhosMatioFile.hpp"
#include "matioHelpers.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
matClassToString(matvar_t* matVariable)
{
    std::string res = "?";
    switch (matVariable->class_type) {
    case MAT_C_SPARSE: {
        if (matVariable->isLogical) {
            res = "logical";
        } else {
            res = "double";
        }
    } break;
    case MAT_C_CELL: {
        res = "cell";
    } break;
    case MAT_C_STRUCT: {
        res = "struct";
    } break;
    case MAT_C_CHAR: {
        res = "char";
    } break;
    case MAT_C_DOUBLE: {
        res = "double";
    } break;
    case MAT_C_SINGLE: {
        res = "single";
    } break;
    case MAT_C_INT8: {
        res = "int8";
    } break;
    case MAT_C_INT16: {
        res = "int16";
    } break;
    case MAT_C_INT32: {
        res = "int32";
    } break;
    case MAT_C_INT64: {
        res = "int64";
    } break;
    case MAT_C_UINT8: {
        if (matVariable->isLogical) {
            res = "logical";
        } else {
            res = "uint8";
        }
    } break;
    case MAT_C_UINT16: {
        res = "uint16";
    } break;
    case MAT_C_UINT32: {
        res = "uint32";
    } break;
    case MAT_C_UINT64: {
        res = "uint64";
    } break;
    case MAT_C_EMPTY:
    case MAT_C_OBJECT:
    case MAT_C_FUNCTION:
    case MAT_C_OPAQUE:
    default: {
        res = "?";
    } break;
    }
    return res;
}
//=============================================================================
static ArrayOf
getNestingEmptyStruct()
{
    stringVector fieldnames;
    fieldnames.push_back("function");
    fieldnames.push_back("level");
    ArrayOfVector values;
    values.push_back(ArrayOf::characterArrayConstructor(""));
    values.push_back(ArrayOf::doubleConstructor(0));
    return ArrayOf::structConstructor(fieldnames, values);
}
//=============================================================================
ArrayOf
WhosMatioFile(
    Interface* io, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    FileSystemWrapper::Path mat_filename(filename);
    bool permissionDenied;
    bool fileExistPreviously
        = FileSystemWrapper::Path::is_regular_file(mat_filename, permissionDenied);
    if (!fileExistPreviously) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    std::string utf8filename = wstring_to_utf8(filename);
    mat_t* matfile = Mat_Open(utf8filename.c_str(), MAT_ACC_RDONLY);
    if (!matfile) {
        Error(_W("Valid .mat file expected."));
    }
    stringVector variableNamesInFile;
    size_t nVars = 0;
    char* const* variableNames = Mat_GetDir(matfile, &nVars);
    for (size_t k = 0; k < nVars; k++) {
        variableNamesInFile.push_back(variableNames[k]);
    }
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (const std::wstring& uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variablesNamesToRead.push_back(name);
            }
        }
    }
    stringVector _names;
    std::vector<Dimensions> _size;
    std::vector<double> _bytes;
    stringVector _class;
    std::vector<logical> _global;
    std::vector<logical> _sparse;
    std::vector<logical> _complex;
    std::vector<logical> _persistent;
    size_t nbSpaceName = _("Name").size();
    size_t nbSpaceSize = _("Size").size();
    size_t nbSpaceBytes = _("Bytes").size();
    size_t nbSpaceClass = _("Class").size();

    _names.reserve(variablesNamesToRead.size());
    _size.reserve(variablesNamesToRead.size());
    _bytes.reserve(variablesNamesToRead.size());
    _class.reserve(variablesNamesToRead.size());
    _global.reserve(variablesNamesToRead.size());
    _sparse.reserve(variablesNamesToRead.size());
    _complex.reserve(variablesNamesToRead.size());
    _persistent.reserve(variablesNamesToRead.size());

    for (const std::string& name : variablesNamesToRead) {
        ArrayOf value;
        matvar_t* matVariable = Mat_VarRead(matfile, name.c_str());
        if (matVariable == nullptr) {
            Mat_Close(matfile);
            std::string msg = _("Cannot read variable:") + std::string(" ") + name;
            Error(msg);
            return {};
        }
        _names.push_back(name);
        nbSpaceName = std::max(nbSpaceName, name.size());

        std::string className = matClassToString(matVariable);
        double sizeAsByte = (double)Mat_VarGetSize(matVariable);
        if ((int)sizeAsByte == (int)0 || className == "?") {
            std::string NaN = "NaN";
            _bytes.push_back(std::nan(NaN.c_str()));
            nbSpaceBytes = std::max(nbSpaceBytes, NaN.size());
        } else {
            _bytes.push_back(sizeAsByte);
            nbSpaceBytes = std::max(nbSpaceBytes, std::to_string((int)sizeAsByte).size());
        }

        _class.push_back(className);
        nbSpaceClass = std::max(nbSpaceClass, className.size());

        Dimensions dims = getMatVarDimensions(matVariable);
        _size.push_back(dims);
        nbSpaceSize = std::max(nbSpaceSize, dims.toString().size());

        _global.push_back(matVariable->isGlobal);
        _sparse.push_back(matVariable->class_type == MAT_C_SPARSE);
        _complex.push_back(matVariable->isComplex != 0);
        _persistent.push_back(false);
        Mat_VarFree(matVariable);
    }
    Mat_Close(matfile);

    if (asStruct) {
        stringVector fieldnames;
        fieldnames.push_back("name");
        fieldnames.push_back("size");
        fieldnames.push_back("bytes");
        fieldnames.push_back("class");
        fieldnames.push_back("global");
        fieldnames.push_back("sparse");
        fieldnames.push_back("complex");
        fieldnames.push_back("nesting");
        fieldnames.push_back("persistent");
        Dimensions dims;
        dims[0] = _names.size();
        dims[1] = 1;
        if (_names.empty()) {
            res = ArrayOf::emptyStructConstructor(fieldnames, dims);
        } else {
            auto* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
            ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
            ArrayOfVector nameArray;
            ArrayOfVector sizeArray;
            ArrayOfVector bytesArray;
            ArrayOfVector classArray;
            ArrayOfVector globalArray;
            ArrayOfVector sparseArray;
            ArrayOfVector complexArray;
            ArrayOfVector nestingArray;
            ArrayOfVector persistentArray;

            nameArray.reserve(dims[0]);
            sizeArray.reserve(dims[0]);
            bytesArray.reserve(dims[0]);
            classArray.reserve(dims[0]);
            globalArray.reserve(dims[0]);
            sparseArray.reserve(dims[0]);
            complexArray.reserve(dims[0]);
            nestingArray.reserve(dims[0]);
            persistentArray.reserve(dims[0]);

            for (indexType i = 0; i < (indexType)_names.size(); ++i) {
                nameArray.push_back(ArrayOf::characterArrayConstructor(_names[i]));
                ArrayOf s = ArrayOf::doubleVectorConstructor(_size[i].getLength());
                double* ptrDouble = (double*)s.getDataPointer();
                for (indexType k = 0; k < _size[i].getLength(); ++k) {
                    ptrDouble[k] = (double)_size[i][k];
                }
                sizeArray.push_back(s);
                bytesArray.push_back(ArrayOf::doubleConstructor(_bytes[i]));
                classArray.push_back(ArrayOf::characterArrayConstructor(_class[i]));
                globalArray.push_back(ArrayOf::logicalConstructor(_global[i]));
                sparseArray.push_back(ArrayOf::logicalConstructor(_sparse[i]));
                complexArray.push_back(ArrayOf::logicalConstructor(_complex[i]));
                nestingArray.push_back(getNestingEmptyStruct());
                persistentArray.push_back(ArrayOf::logicalConstructor(_persistent[i]));
            }

            st.setFieldAsList("name", nameArray);
            st.setFieldAsList("size", sizeArray);
            st.setFieldAsList("bytes", bytesArray);
            st.setFieldAsList("class", classArray);
            st.setFieldAsList("global", globalArray);
            st.setFieldAsList("sparse", sparseArray);
            st.setFieldAsList("complex", complexArray);
            st.setFieldAsList("nesting", nestingArray);
            st.setFieldAsList("persistent", persistentArray);
            res = st;
        }
    } else {
        if (_names.size()) {
            std::stringstream ssLine;
            ssLine << "  " << std::left << std::setfill(' ') << std::setw(nbSpaceName) << _("Name");
            ssLine << "      " << std::left << std::setfill(' ') << std::setw(nbSpaceSize)
                   << _("Size");
            ssLine << "            " << std::right << std::setfill(' ') << std::setw(nbSpaceBytes)
                   << _("Bytes");
            ssLine << "  " << std::left << std::setfill(' ') << std::setw(nbSpaceClass)
                   << _("Class");
            ssLine << "  " << _("Attributes");
            ssLine << std::endl;
            ssLine << std::endl;
            io->outputMessage(ssLine.str());
            for (indexType i = 0; i < (indexType)_names.size(); ++i) {
                ssLine.str("");

                std::string bytes = std::isnan(_bytes[i]) ? "-" : std::to_string((int)_bytes[i]);

                ssLine << "  " << std::left << std::setfill(' ') << std::setw(nbSpaceName)
                       << _names[i];
                ssLine << "      " << std::left << std::setfill(' ') << std::setw(nbSpaceSize)
                       << _size[i].toString();
                ssLine << "            " << std::right << std::setfill(' ')
                       << std::setw(nbSpaceBytes) << bytes;
                ssLine << "  " << std::left << std::setfill(' ') << std::setw(nbSpaceClass)
                       << _class[i];
                bool first = true;
                if (_global[i]) {
                    ssLine << "  "
                           << "global";
                    first = false;
                }
                if (_persistent[i]) {
                    if (!first) {
                        ssLine << ", persistent";
                    } else {
                        ssLine << "  persistent";
                    }
                    first = false;
                }
                if (_sparse[i]) {
                    if (!first) {
                        ssLine << ", sparse";
                    } else {
                        ssLine << "   sparse";
                    }
                    first = false;
                }
                if (_complex[i]) {
                    if (!first) {
                        ssLine << ", complex";
                    } else {
                        ssLine << "   complex";
                    }
                    first = false;
                }
                ssLine << std::endl;
                io->outputMessage(ssLine.str());
            }
            io->outputMessage("\n");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
