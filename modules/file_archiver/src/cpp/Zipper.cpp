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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <zlib.h>
#include <algorithm>
#include <sstream>
#include <ctime>
#include <mz_os.h>
#include "Zipper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const unsigned int BUFSIZE = 8192;
//=============================================================================
Zipper::Zipper() = default;
//=============================================================================
Zipper::~Zipper() { close(); }
//=============================================================================
bool
Zipper::open(const char* filename, bool append)
{
    close();
    m_zipFile = zipOpen64(filename, append ? APPEND_STATUS_ADDINZIP : 0);
    return isOpen();
}
//=============================================================================
void
Zipper::close()
{
    if (m_zipFile != nullptr) {
        closeEntry();
        zipClose(m_zipFile, nullptr);
        m_zipFile = nullptr;
    }
}
//=============================================================================
bool
Zipper::isOpen()
{
    return m_zipFile != nullptr;
}
//=============================================================================
bool
Zipper::addEntry(const char* filename, uint32_t attributes)
{
    if (isOpen()) {
        closeEntry();
        zip_fileinfo zi = { 0 };
        getTime(zi.tmz_date);
        zi.dosDate = 0;
        zi.external_fa = attributes;
        int err = zipOpenNewFileInZip4_64(m_zipFile, filename, &zi, NULL, 0, NULL, 0, NULL,
            Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0, 0, 0, 0, NULL, 0, MZ_VERSION_MADEBY, 1 << 11, 0);
        m_entryOpen = (err == ZIP_OK);
    }
    return m_entryOpen;
}
//=============================================================================
void
Zipper::closeEntry()
{
    if (m_entryOpen) {
        zipCloseFileInZip(m_zipFile);
        m_entryOpen = false;
    }
}
//=============================================================================
bool
Zipper::isOpenEntry()
{
    return m_entryOpen;
}
//=============================================================================
Zipper&
Zipper::operator<<(std::istream& is)
{
    int err = ZIP_OK;
    char buf[BUFSIZE];
    unsigned long nRead = 0;

    if (isOpenEntry()) {
        while (err == ZIP_OK && is.good()) {
            is.read(buf, BUFSIZE);
            unsigned int nRead = (unsigned int)is.gcount();

            if (nRead) {
                err = zipWriteInFileInZip(m_zipFile, buf, nRead);
            } else {
                break;
            }
        }
    }
    return *this;
}
//=============================================================================
void
Zipper::getTime(tm_zip& tmZip)
{
#ifdef _MSC_VER
    __time64_t rawtime;
    _time64(&rawtime);
    auto timeinfo = _localtime64(&rawtime);
#else
    time_t rawtime;
    time(&rawtime);
    struct tm t_result;
    auto timeinfo = localtime_r(&rawtime, &t_result);
#endif
    tmZip.tm_sec = timeinfo->tm_sec;
    tmZip.tm_min = timeinfo->tm_min;
    tmZip.tm_hour = timeinfo->tm_hour;
    tmZip.tm_mday = timeinfo->tm_mday;
    tmZip.tm_mon = timeinfo->tm_mon;
    tmZip.tm_year = timeinfo->tm_year;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
