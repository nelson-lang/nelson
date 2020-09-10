//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <iostream>
#include <iomanip>
#include <sstream>
#include <zlib.h>
#include <cstring>
#include "StringZLib.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_SIZE 8192
//=============================================================================
std::string
compressString(const std::string& str, bool& failed)
{
    std::string outstring;
    z_stream zs;
    memset(&zs, 0, sizeof(zs));
    if (deflateInit(&zs, Z_BEST_SPEED) != Z_OK) {
        failed = true;
        return std::string();
    }

    zs.next_in = (Bytef*)str.data();
    zs.avail_in = (uInt)str.size();

    int ret = Z_OK;
    char outbuffer[BUFFER_SIZE];
    do {
        zs.next_out = reinterpret_cast<Bytef*>(outbuffer);
        zs.avail_out = sizeof(outbuffer);

        ret = deflate(&zs, Z_FINISH);
        outstring.reserve(zs.total_out);
        if (outstring.size() < zs.total_out) {
            outstring.append(outbuffer, zs.total_out - outstring.size());
        }
    } while (ret == Z_OK);

    deflateEnd(&zs);

    if (ret != Z_STREAM_END) {
        failed = true;
        return std::string();
    }
    failed = false;
    return outstring;
}
//=============================================================================
std::string
decompressString(const std::string& str, bool& failed)
{
    std::string outstring;
    z_stream zs;
    memset(&zs, 0, sizeof(zs));

    if (inflateInit(&zs) != Z_OK) {
        failed = true;
        return std::string();
    }

    zs.next_in = (Bytef*)str.data();
    zs.avail_in = (uInt)str.size();

    int ret = Z_OK;
    char outbuffer[BUFFER_SIZE];
    do {
        zs.next_out = reinterpret_cast<Bytef*>(outbuffer);
        zs.avail_out = sizeof(outbuffer);

        ret = inflate(&zs, 0);
        if (outstring.capacity() < zs.total_out) {
            outstring.reserve(zs.total_out);
        }
        if (outstring.size() < zs.total_out) {
            outstring.append(outbuffer, zs.total_out - outstring.size());
        }

    } while (ret == Z_OK);

    inflateEnd(&zs);

    if (ret != Z_STREAM_END) {
        failed = true;
        return std::string();
    }
    failed = false;
    return outstring;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
