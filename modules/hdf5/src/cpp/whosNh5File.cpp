//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <iomanip>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/container/vector.hpp>
#include "whosNh5File.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5LoadVariable.hpp"
#include "characters_encoding.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
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
whosNh5File(Interface* io, const std::wstring& filename, const wstringVector& names, bool asStruct)
{
    ArrayOf res;
    boost::filesystem::path nh5_filename(filename);
    bool fileExistPreviously = false;
    try {
        fileExistPreviously = boost::filesystem::exists(nh5_filename)
            && !boost::filesystem::is_directory(nh5_filename);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            Error(_W("Permission denied."));
        }
        fileExistPreviously = false;
    }
    if (!fileExistPreviously) {
        Error(_W("File does not exist."));
    }

    hid_t fid
        = H5Fopen(wstring_to_utf8(nh5_filename.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid == H5I_INVALID_HID) {
        Error(_W("Open file failed."));
    }
    if (!isNelsonH5File(fid)) {
        H5Fclose(fid);
        Error(_W("Invalid file format."));
    }
    stringVector variableNamesInFile = getVariableNames(fid);
    stringVector variablesNamesToRead;
    if (names.empty()) {
        variablesNamesToRead = variableNamesInFile;
    } else {
        for (std::wstring uname : names) {
            std::string name = wstring_to_utf8(uname);
            if (std::find(variableNamesInFile.begin(), variableNamesInFile.end(), name)
                != variableNamesInFile.end()) {
                variablesNamesToRead.push_back(name);
            }
        }
    }
    stringVector _names;
    boost::container::vector<Dimensions> _size;
    boost::container::vector<double> _bytes;
    stringVector _class;
    boost::container::vector<logical> _global;
    boost::container::vector<logical> _sparse;
    boost::container::vector<logical> _complex;
    boost::container::vector<logical> _persistent;
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

    for (std::string name : variablesNamesToRead) {
        ArrayOf value;
        if (h5LoadVariable(fid, "/", name, value)) {
            _names.push_back(name);
            nbSpaceName = std::max(nbSpaceName, name.size());

            std::string className = ClassName(value);
            double sizeAsByte;
            if (value.isSparse()) {
                sizeAsByte = std::nan("NaN");
            } else {
                sizeAsByte = (double)value.getByteSize();
            }
            _bytes.push_back(sizeAsByte);
            nbSpaceBytes = std::max(nbSpaceBytes, std::to_string((int)sizeAsByte).size());

            _class.push_back(className);
            nbSpaceClass = std::max(nbSpaceClass, className.size());

            Dimensions dims = value.getDimensions();
            _size.push_back(dims);
            nbSpaceSize = std::max(nbSpaceSize, dims.toString().size());

            _global.push_back(0);
            _sparse.push_back(value.isSparse());
            _complex.push_back(value.isComplex());
            _persistent.push_back(false);
        } else {
            H5Fclose(fid);
            std::string msg = _("Cannot read variable:") + std::string(" ") + name;
            Error(msg);
        }
    }
    herr_t status = H5Fclose(fid);

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
            auto* elements = static_cast<ArrayOf*>(
                ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames));
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

            for (indexType i = 0; i < _names.size(); ++i) {
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
            for (indexType i = 0; i < _names.size(); ++i) {
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
