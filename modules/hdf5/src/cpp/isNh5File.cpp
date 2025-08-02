//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "isNh5File.hpp"
#include "characters_encoding.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "haveNh5Header.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
isNh5File(const wstringVector& filenames, ArrayOf& results, ArrayOf& versions, ArrayOf& headers)
{
    Dimensions dims(filenames.size(), 1);
    logical* res
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, filenames.size(), stringVector(), false);
    results = ArrayOf(NLS_LOGICAL, dims, res);
    if (filenames.size() == 0) {
        versions = ArrayOf::stringArrayConstructor(filenames, dims);
        headers = ArrayOf::stringArrayConstructor(filenames, dims);
        return;
    }
    ArrayOf* elementVersions = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, filenames.size(), stringVector(), false);
    versions = ArrayOf(NLS_STRING_ARRAY, dims, elementVersions);

    ArrayOf* elementHeaders = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, filenames.size(), stringVector(), false);
    headers = ArrayOf(NLS_STRING_ARRAY, dims, elementHeaders);

    for (size_t k = 0; k < filenames.size(); ++k) {
        std::wstring filename = filenames[k];
        FileSystemWrapper::Path mat_filename(filename);
        bool fileExistPreviously = FileSystemWrapper::Path::is_regular_file(mat_filename);
        if (!fileExistPreviously) {
            res[k] = false;
            elementVersions[k] = ArrayOf::characterArrayConstructor("");
            elementHeaders[k] = ArrayOf::characterArrayConstructor("");
        } else {
            std::wstring nh5Header;
            int16 nh5Version;
            int16 nh5Endian;
            bool haveHeader = haveNh5Header(filename, nh5Header, nh5Version, nh5Endian);
            if (haveHeader) {
                res[k] = true;
                if (nh5Version == NELSON_HEADER_VERSION) {
                    elementVersions[k] = ArrayOf::characterArrayConstructor("-v1");
                } else {
                    elementVersions[k] = ArrayOf::characterArrayConstructor("");
                }
                elementHeaders[k] = ArrayOf::characterArrayConstructor(nh5Header);
            } else {
                std::string utf8filename = wstring_to_utf8(filename);
                hid_t fid = H5Fopen(utf8filename.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
                if (fid == H5I_INVALID_HID) {
                    res[k] = false;
                    ArrayOf empty = ArrayOf::emptyConstructor(0, 1);
                    empty.promoteType(NLS_CHAR);
                    elementVersions[k] = ArrayOf::characterArrayConstructor("");
                    elementHeaders[k] = ArrayOf::characterArrayConstructor("");
                } else {
                    res[k] = isNelsonH5File(fid);
                    if (res[k]) {
                        elementVersions[k] = ArrayOf::characterArrayConstructor("-v1");
                    } else {
                        elementVersions[k] = ArrayOf::characterArrayConstructor("");
                    }
                    elementHeaders[k] = ArrayOf::characterArrayConstructor(NELSON_HEADER);
                    H5Fclose(fid);
                }
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
