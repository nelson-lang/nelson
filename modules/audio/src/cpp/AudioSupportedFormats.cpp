//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include <cstring>
#include <sndfile.h>
#include "AudioSupportedFormats.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
stringVector
AudioSubFormats(int format)
{
    stringVector subFormats;
    int countSubFormats;
    sf_command(NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &countSubFormats, sizeof(int));
    for (int i = 0; i < countSubFormats; i++) {
        SF_FORMAT_INFO info;
        info.format = i;
        sf_command(NULL, SFC_GET_FORMAT_SUBTYPE, &info, sizeof(info));
        SF_INFO sfinfo;
        memset(&sfinfo, 0, sizeof(sfinfo));
        sfinfo.channels = 1;
        sfinfo.format = (format & SF_FORMAT_TYPEMASK) | info.format;
        if (sf_format_check(&sfinfo)) {
            subFormats.push_back(info.name);
        }
    }
    return subFormats;
}
//=============================================================================
ArrayOf
AudioSupportedFormats()
{
    int countFormat;
    sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT, &countFormat, sizeof(int));
    ArrayOfVector formats;
    ArrayOfVector extensions;
    ArrayOfVector subformats;
    formats.reserve(countFormat);
    extensions.reserve(countFormat);
    subformats.reserve(countFormat);
    Dimensions dims;
    dims[0] = countFormat;
    dims[1] = 1;
    stringVector fieldnames;
    fieldnames.push_back("Name");
    fieldnames.push_back("Extension");
    fieldnames.push_back("Subformats");
    if (countFormat == 0) {
        return ArrayOf::emptyStructConstructor(fieldnames, dims);
    }
    ArrayOf* elements
        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
    ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    for (int i = 0; i < countFormat; i++) {
        SF_FORMAT_INFO info;
        info.format = i;
        sf_command(NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof(info));
        formats.push_back(ArrayOf::characterArrayConstructor(info.name));
        std::string extension = std::string(info.extension);
        if (extension[0] != '.') {
            extension = "." + extension;
        }
        extensions.push_back(ArrayOf::characterArrayConstructor(extension));
        subformats.push_back(ToCellStringAsColumn(AudioSubFormats(info.format)));
    }
    st.setFieldAsList("Name", formats);
    st.setFieldAsList("Extension", extensions);
    st.setFieldAsList("Subformats", subformats);
    return st;
}
//=============================================================================
}
//=============================================================================
