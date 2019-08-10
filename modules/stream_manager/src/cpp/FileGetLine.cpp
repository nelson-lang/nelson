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
#include <cstring>
#include <cstdio>
#include <memory>
#include <stdexcept>
#include "FileGetLine.hpp"
#include "FileSeek.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_LINE_SIZE (4096 * 4)
//=============================================================================
bool
FileGetLine(File* fp, int nchar, bool bWithNewLine, std::wstring& result)
{
    bool bOK = false;
    result.clear();
    if (!fp) {
        return false;
    }
    if (fp->isInterfaceMethod()) {
        return false;
    }
    FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
    if (!fileptr) {
        return false;
    }
    if (feof(fileptr) || ferror(fileptr)) {
        return false;
    }
    bool bEOF = false;
    bool haveError = false;
    std::string readline;
    char buffer[BUFFER_LINE_SIZE];
    memset(buffer, '\0', sizeof(char) * BUFFER_LINE_SIZE);
    bool continueToReadLine = true;
    do {
        if (feof(fileptr)) {
            bEOF = true;
            continueToReadLine = false;
            break;
        }
        if (ferror(fileptr)) {
            haveError = true;
            continueToReadLine = false;
            break;
        }
        if (fgets(buffer, BUFFER_LINE_SIZE, fileptr)) {
            readline.append(buffer);
            memset(buffer, '\0', sizeof(char) * BUFFER_LINE_SIZE);
            if (readline.length() > 0) {
                int sizeRemove = 1;
                size_t index = readline.find('\r');
                if (index != std::string::npos) {
                    if (readline.length() > index + 1 && readline[index + 1] == '\n') {
                        sizeRemove = 2;
                    } else {
                        size_t temp = readline.find('\n');
                        if (temp != std::string::npos && temp < index) {
                            index = temp;
                        }
                    }
                } else {
                    index = readline.find('\n');
                }
                if (index != std::string::npos) {
                    index += sizeRemove;
                    FileSeek(fp, static_cast<int64>(index - readline.length()), 0);
                    readline.erase(readline.begin() + index, readline.end());
                    continueToReadLine = false;
                }
            } else {
                continueToReadLine = false;
            }
        } else {
            continueToReadLine = false;
        }
    } while (continueToReadLine);

    if (haveError) {
        bOK = false;
    } else {
        if (!bWithNewLine) {
            if (readline.length() > 0) {
                if (readline[readline.length() - 1] == '\n') {
                    readline.pop_back();
                    if (readline.length() > 0) {
                        if (readline[readline.length() - 1] == '\r') {
                            readline.pop_back();
                        }
                    }
                }
            }
        }
        if (nchar == 0) {
            result.clear();
            return true;
        } else {
            std::string encoding = wstring_to_utf8(fp->getEncoding());
            if (encoding == "UTF-8") {
                result = utf8_to_wstring(readline);
            } else {
                std::string asUtf8;
                bOK = charsetToUtf8Converter(readline, encoding, asUtf8);
                result = utf8_to_wstring(asUtf8);
            }
            if (nchar > 0) {
                if (nchar < static_cast<indexType>(result.length())) {
                    std::wstring w = result;
                    result.resize((size_t)(nchar));
                    try {
                        w = w.substr((size_t)(nchar));
                    } catch (const std::out_of_range&) {
                    }
                    std::string u = wstring_to_utf8(w);
                    auto nseek = static_cast<int64>(u.length());
                    FileSeek(fp, -nseek, 0);
                }
                bOK = true;
            } else if (nchar == -1) {
                bOK = true;
            } else {
                bOK = false;
            }
        }
    }
    return bOK;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
