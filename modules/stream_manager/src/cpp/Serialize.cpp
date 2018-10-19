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
#ifdef _MSC_VER
#include <winsock2.h>
#else
#include <netinet/in.h>
#endif
#include "Error.hpp"
#include "Serialize.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Serialize::Serialize(Stream* sck) { s = sck; }
//=============================================================================
Serialize::~Serialize() { s = nullptr; }
//=============================================================================
void
Serialize::sendSignature(const char sig, int count)
{
    s->writeBytes(&sig, 1);
    long netcount = htonl(count);
    s->writeBytes(&netcount, sizeof(long));
}
//=============================================================================
void
Serialize::checkSignature(const char sig, int count)
{
    char rtype = 0;
    s->readBytes(&rtype, 1);
    long rcount = 0;
    s->readBytes(&rcount, sizeof(long));
    rcount = ntohl(rcount);
    if (!((sig == rtype) && (count == rcount))) {
        char buffer[406];
        sprintf(buffer, "Serialization Mismatch: expected <%c, %d>, got <%c, %ld>", sig, count,
            rtype, rcount);
        Error(buffer);
    }
}
//=============================================================================
void
Serialize::putBool(bool b)
{
    if (b) {
        putByte(1);
    } else {
        putByte(0);
    }
}
//=============================================================================
void
Serialize::putBools(const bool* ptr, int count)
{}
//=============================================================================
void
Serialize::putByte(char b)
{
    putBytes(&b, 1);
}
//=============================================================================
void
Serialize::putBytes(const char* ptr, int count)
{
    sendSignature('c', count);
    s->writeBytes(ptr, count * sizeof(char));
}
//=============================================================================
void
Serialize::putInt(int i)
{
    putInts(&i, 1);
}
//=============================================================================
void
Serialize::putInts(const int* i, int count)
{
    sendSignature('i', count);
    s->writeBytes(i, count * sizeof(int));
}
//=============================================================================
void
Serialize::putString(std::string str)
{
    unsigned int len;
    sendSignature('x', 0);
    if ((str.empty())) {
        len = 0;
        putInts((int*)&len, 1);
    } else {
        len = str.length() + 1;
        putInts((int*)&len, 1);
        putBytes(str.c_str(), len);
    }
}
//=============================================================================
void
Serialize::putWString(std::wstring wstr)
{
    putString(wstring_to_utf8(wstr));
}
//=============================================================================
void
Serialize::putStringVector(stringVector vstr)
{
    sendSignature('S', 1);
    putInt(vstr.size());
    for (size_t i = 0; i < vstr.size(); i++) {
        putString(vstr[i].c_str());
    }
}
//=============================================================================
void
Serialize::putWStringVector(wstringVector vwstr)
{
    sendSignature('S', 1);
    putInt(vwstr.size());
    for (size_t i = 0; i < vwstr.size(); i++) {
        putWString(vwstr[i].c_str());
    }
}
//=============================================================================
bool
Serialize::getBool()
{
    char b = getByte();
    return (b == 1);
}
//=============================================================================
void
Serialize::getBools(bool* b, int count)
{}
//=============================================================================
char
Serialize::getByte()
{
    char b = 0;
    getBytes(&b, 1);
    return b;
}
//=============================================================================
void
Serialize::getBytes(char* b, int count)
{
    checkSignature('c', count);
    s->readBytes(b, count * sizeof(char));
}
//=============================================================================
int
Serialize::getInt()
{
    int t = 0;
    getInts(&t, 1);
    return t;
}
//=============================================================================
void
Serialize::getInts(int* i, int count)
{
    checkSignature('i', count);
    s->readBytes(i, count * sizeof(int));
}
//=============================================================================
std::string
Serialize::getString()
{
    std::string res;
    checkSignature('x', 0);
    unsigned int len = 0;
    getInts((int*)&len, 1);
    if (len == 0) {
        return res;
    }
    char* cp;
    try {
        cp = new char[len];
    } catch (const std::bad_alloc&) {
        cp = nullptr;
    }
    if (cp) {
        getBytes(cp, len);
        res = cp;
        delete[] cp;
    }
    return res;
}
//=============================================================================
std::wstring
Serialize::getWString()
{
    return utf8_to_wstring(getString());
}
//=============================================================================
stringVector
Serialize::getStringVector()
{
    stringVector res;
    checkSignature('S', 1);
    int L = getInt();
    res.reserve(L);
    for (size_t i = 0; i < L; i++) {
        res.push_back(getString());
    }
    return res;
}
//=============================================================================
wstringVector
Serialize::getWStringVector()
{
    wstringVector res;
    checkSignature('S', 1);
    int L = getInt();
    res.reserve(L);
    for (size_t i = 0; i < L; i++) {
        res.push_back(getWString());
    }
    return res;
}
//=============================================================================
}
//=============================================================================
