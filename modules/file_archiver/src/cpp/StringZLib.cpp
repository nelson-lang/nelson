//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
        return {};
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
        return {};
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
        return {};
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
        return {};
    }
    failed = false;
    return outstring;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
