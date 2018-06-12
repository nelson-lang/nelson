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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Stream.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
class NLSSTREAM_MANAGER_IMPEXP Serialize
{
private:
    Stream* s;

public:
    Serialize(Stream*);
    ~Serialize();

    // Send a stream of base objects
    // Check a signature
    void
    sendSignature(const char sig, int count);

    void
    putByte(char b);
    void
    putBytes(const char* ptr, int count);

    void
    putBool(bool b);
    void
    putBools(const bool* b, int count);

    void
    putInt(int i);
    void
    putInts(const int* i, int count);

    void
    putString(std::string str);
    void
    putWString(std::wstring wstr);
    void
    putStringVector(stringVector vstr);
    void
    putWStringVector(wstringVector vwstr);

    // Send a signature
    void
    checkSignature(const char sig, int count);

    bool
    getBool();
    void
    getBools(bool* b, int count);

    char
    getByte();
    void
    getBytes(char* b, int count);

    int
    getInt();
    void
    getInts(int* ptr, int count);

    std::string
    getString();
    std::wstring
    getWString();
    stringVector
    getStringVector();
    wstringVector
    getWStringVector();
};
} // namespace Nelson
  //=============================================================================