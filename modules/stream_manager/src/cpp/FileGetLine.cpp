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
#include <stdexcept>
#include "FileGetLine.hpp"
#include "FileSeek.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_LINE_SIZE 4096
//=============================================================================
bool
FileGetLine(File* fp, int nchar, bool bWithNewLine, std::wstring& result)
{
    bool bOK = false;
    result.clear();
    if (fp) {
        if (!fp->isInterfaceMethod()) {
            FILE* fileptr = (FILE*)fp->getFilePointer();
            if (fileptr) {
                bool bEOF = true;
                std::string readline;
                char buffer[BUFFER_LINE_SIZE];
                strcpy(buffer, "");
                char* read = fgets(buffer, BUFFER_LINE_SIZE, fileptr);
                while (read != nullptr) {
                    readline += buffer;
                    if (readline.length() > 0) {
                        int sizeRemove = 1;
                        size_t index = readline.find("\r");
                        if (index != std::string::npos) {
                            if (readline.length() > index + 1 && readline[index + 1] == '\n') {
                                sizeRemove = 2;
                            } else {
                                size_t temp = readline.find("\n");
                                if (temp != std::string::npos && temp < index) {
                                    index = temp;
                                }
                            }
                        } else {
                            index = readline.find("\n");
                        }
                        if (index == std::string::npos) {
                            if (!feof(fileptr)) {
                                read = fgets(buffer, BUFFER_LINE_SIZE, fileptr);
                            } else {
                                bEOF = false;
                                break;
                            }
                        } else {
                            index += sizeRemove;
                            FileSeek(fp, (int64)(index - readline.length()), 0);
                            readline.erase(readline.begin() + index, readline.end());
                            bEOF = false;
                            break;
                        }
                    } else {
                        bEOF = true;
                        break;
                    }
                }
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
                if (bEOF) {
                    bOK = false;
                } else {
                    if (nchar == 0) {
                        result = L"";
                        bOK = true;
                    } else if (nchar > 0) {
                        result = utf8_to_wstring(readline);
                        if (nchar < (indexType)result.length()) {
                            std::wstring w = result;
                            result.resize(nchar);
                            try {
                                w = w.substr(nchar);
                            } catch (const std::out_of_range&) {
                            }
                            std::string u = wstring_to_utf8(w);
                            int64 nseek = (int64)u.length();
                            FileSeek(fp, -nseek, 0);
                        }
                        bOK = true;
                    } else if (nchar == -1) {
                        result = utf8_to_wstring(readline);
                        bOK = true;
                    } else {
                        bOK = false;
                    }
                }
            }
        }
    }
    return bOK;
}
//=============================================================================
}
//=============================================================================
