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
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
#pragma once
//=============================================================================
#include <cstring>
#include <vector>
#include "nlsError_manager_exports.h"
#include "Interface.hpp"
#include "PositionScript.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
/**
 * The exception class.  This is a minimal class for now that
 * allows for a hierarchical error structure (if desired) later
 * on.  Since we simply print most messages to the console,
 * the exception types are not encoded using RTTI...
 */
class NLSERROR_MANAGER_IMPEXP Exception
{
private:
    std::vector<PositionScript> backtrace;
    std::wstring identifier = L"";
    std::wstring msg = L"";

public:
    Exception(const std::string& msg_in, std::vector<PositionScript> positions,
        const std::string& identifier_in = "");
    Exception(const std::wstring& msg_in, std::vector<PositionScript> positions,
        const std::wstring& identifier_in = L"");
    Exception(const std::string& msg_in, const PositionScript& position,
        const std::string& identifier_in = "");
    Exception(const std::wstring& msg_in, const PositionScript& position,
        const std::wstring& identifier_in = L"");
    Exception(const std::string& msg_in, const std::string& identifier_in = "");
    Exception(const std::wstring& msg_in, const std::wstring& identifier_in = L"");
    Exception();

    /**
     * Copy constructor.
     */
    Exception(const Exception& copy);
    /**
     * Assignment operator.
     */
    void
    operator=(const Exception& copy);
    /**
     * Standard destructor.
     */
    ~Exception();
    /**
     * Output the contents of the exception to the console.
     */
    void
    printMe(Interface* io);
    /**
     * compares messages
     */
    bool
    matches(const std::string& tst_msg);
    bool
    matches(const std::wstring& tst_msg);
    /**
     * Get the message member function.
     */
    std::wstring
    getMessage();

    void
    setIdentifier(const std::wstring& identifier_in);
    void
    setIdentifier(const std::string& identifier_in);

    std::wstring
    getFormattedErrorMessage();
    std::wstring
    what()
    {
        return msg;
    }

    std::wstring
    getFilename();

    int
    getLine();

    std::wstring
    getFunctionName();

    bool
    isEmpty();

    std::wstring
    getIdentifier();

    std::vector<PositionScript>
    getTrace();
};
//=============================================================================
template <class T>
T*
new_with_exception(size_t len, bool initializeToZero = true)
{
    T* ptr = nullptr;
    if (len != 0) {
        try {
            ptr = new T[len];
            if (initializeToZero) {
                memset(ptr, 0, sizeof(T) * len);
            }
        } catch (const std::bad_alloc& e) {
            e.what();
            ptr = nullptr;
            Error(ERROR_MEMORY_ALLOCATION);
        }
    }
    return ptr;
}
//=============================================================================
}
//=============================================================================
