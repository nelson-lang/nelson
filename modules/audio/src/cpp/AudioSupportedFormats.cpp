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
#include <sndfile.h>
#include "AudioSupportedFormats.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
stringVector
AudioSubFormats(int format)
{
    stringVector subFormats;
    int countSubFormats;
    sf_command(nullptr, SFC_GET_FORMAT_SUBTYPE_COUNT, &countSubFormats, sizeof(int));
    for (int i = 0; i < countSubFormats; i++) {
        SF_FORMAT_INFO info;
        info.format = i;
        sf_command(nullptr, SFC_GET_FORMAT_SUBTYPE, &info, sizeof(info));
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
    sf_command(nullptr, SFC_GET_FORMAT_MAJOR_COUNT, &countFormat, sizeof(int));
    ArrayOfVector formats;
    ArrayOfVector extensions;
    ArrayOfVector subformats;
    formats.reserve((size_t)countFormat);
    extensions.reserve((size_t)countFormat);
    subformats.reserve((size_t)countFormat);
    Dimensions dims;
    dims[0] = (indexType)countFormat;
    dims[1] = (indexType)1;
    stringVector fieldnames;
    fieldnames.reserve(3);
    fieldnames.push_back("Name");
    fieldnames.push_back("Extension");
    fieldnames.push_back("Subformats");
    if (countFormat == 0) {
        return ArrayOf::emptyStructConstructor(fieldnames, dims);
    }
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false);
    ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
    for (int i = 0; i < countFormat; i++) {
        SF_FORMAT_INFO info;
        info.format = i;
        sf_command(nullptr, SFC_GET_FORMAT_MAJOR, &info, sizeof(info));
        formats.push_back(ArrayOf::characterArrayConstructor(info.name));
        std::string extension = std::string(info.extension);
        if (extension[0] != '.') {
            extension = "." + extension;
        }
        extensions << ArrayOf::characterArrayConstructor(extension);
        subformats << ArrayOf::toCellArrayOfCharacterColumnVectors(AudioSubFormats(info.format));
    }
    st.setFieldAsList("Name", formats);
    st.setFieldAsList("Extension", extensions);
    st.setFieldAsList("Subformats", subformats);
    return st;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
