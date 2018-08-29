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
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "FileWrite.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FWRITE_ERROR_TYPE
FileWrite(Evaluator* eval, File* fp, ArrayOf src, Class destClass, size_t skip,
    bool bIsLittleEndian, int& sizeWritten)
{
    FWRITE_ERROR_TYPE fwrite_error = FWRITE_NO_ERROR;
    sizeWritten = -1;
    if (fp) {
        switch (src.getDataClass()) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            ArrayOf toWrite(src);
            toWrite.promoteType(destClass);
            void* dp = toWrite.getReadWriteDataPointer();
            if (fp->isInterfaceMethod()) {
                Interface* io = (Interface*)fp->getFilePointer();
                if (skip != 0) {
                    fwrite_error = FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED;
                } else if ((fp->getFileName() == L"stdout") || (fp->getFileName() == L"stderr")) {
                    toWrite.promoteType(NLS_CHAR);
                    std::string str = toWrite.getContentAsCString();
                    if (bIsLittleEndian != isLittleEndianFormat()) {
                        for (size_t k = 0; k < str.size(); k++) {
                            str[k] = bswap<char>(str[k]);
                        }
                    }
                    if (fp->getFileName() == L"stdout") {
                        io->outputMessage(str);
                    } else {
                        io->errorMessage(str);
                    }
                    sizeWritten = (int)str.size();
                    fwrite_error = FWRITE_NO_ERROR;
                } else {
                    fwrite_error = FWRITE_FILE_DESTINATION_NOT_SUPPORTED;
                }
            } else {
                FILE* filepointer = (FILE*)fp->getFilePointer();
                if (filepointer) {
                    if (skip) {
                        char* skipdata;
                        try {
                            skipdata = new char[skip];
                        } catch (const std::bad_alloc& e) {
                            e.what();
                            skipdata = nullptr;
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        if (skipdata) {
                            memset(skipdata, 0, skip);
                            fwrite(skipdata, sizeof(char), skip, filepointer);
                            delete[] skipdata;
                            skipdata = nullptr;
                        }
                    }
                    size_t written = 0;
                    if ((destClass == src.getDataClass()) && destClass == NLS_CHAR) {
                        std::string str = toWrite.getContentAsCString();
                        if (bIsLittleEndian != isLittleEndianFormat()) {
                            for (size_t k = 0; k < str.size(); k++) {
                                str[k] = bswap<char>(str[k]);
                            }
                        }
                        written = fwrite(str.c_str(), sizeof(char), str.size(), filepointer);
                    } else {
                        size_t count(toWrite.getLength());
                        size_t elsize(toWrite.getElementSize());
                        if (bIsLittleEndian != isLittleEndianFormat()) {
                            BITSWAP(dp, count, destClass);
                        }
                        written = fwrite(dp, elsize, count, filepointer);
                    }
                    sizeWritten = (int)written;
                }
                fwrite_error = FWRITE_NO_ERROR;
            }
        } break;
        default: {
            fwrite_error = FWRITE_DATA_TYPE_NOT_SUPPORTED;
        } break;
        }
    } else {
        fwrite_error = FWRITE_INVALID_FILE;
    }
    return fwrite_error;
}
//=============================================================================
};
//=============================================================================
