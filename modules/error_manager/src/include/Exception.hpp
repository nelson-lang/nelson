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
#include <string>
#include <cstring>
#include "nlsError_manager_exports.h"
#include "Interface.hpp"
#include "Messages.hpp"
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
    class NLSERROR_MANAGER_IMPEXP Exception {
    private:
        std::wstring functionname = L"";
        std::wstring msg = L"";
        int line = -1;
        int position = -1;
        std::wstring filename = L"";
        std::wstring identifier = L"";
    public:
        /**
         * Construct an exception object with a given STL-string.
         */
        Exception(std::string msg_in, std::string functionname = "", int line_in = -1, int position_in = -1, std::string filename_in = "", std::string identifier_in = "");
        Exception(std::wstring msg_in, std::wstring functionname = L"", int line_in = -1, int position_in = -1, std::wstring filename_in = L"", std::wstring identifier_in = L"");

        /**
         * Copy constructor.
         */
        Exception(const Exception& copy);
        /**
         * Assignment operator.
         */
        void operator=(const Exception &copy);
        /**
         * Standard destructor.
         */
        ~Exception();
        /**
         * Output the contents of the exception to the console.
         */
        void printMe(Interface *io);
        /**
         * compares messages
         */
        bool matches(std::string tst_msg);
        bool matches(std::wstring tst_msg);
        /**
         * Get the message member function.
        */
        std::wstring getMessage();
        std::wstring getFormattedErrorMessage();
        std::wstring what() {
            return msg;
        }
        std::wstring getFilename() {
            return filename;
        }

        int getLine();
        int getPosition();
        void setLinePosition(int line_in, int position_in);
        void setMessage(std::string msg_in);
        void setMessage(std::wstring msg_in);

        void setFunctionName(std::wstring functionname);
        void setFunctionName(std::string functionname);

        void setFileName(std::wstring filename);

        std::wstring getFunctionName();

        bool isEmpty();

        std::wstring getIdentifier();
        void setIdentifier(std::wstring identifier_in);
        void setIdentifier(std::string identifier_in);

    };

    void printExceptionCount();
    //=============================================================================
    template <class T> T* new_with_exception(size_t len, bool initializeToZero = true)
    {
        T* ptr = nullptr;
        if (len != 0)
        {
            try
            {
                ptr = new T[len];
                if (initializeToZero)
                {
                    memset(ptr, 0, sizeof(T)*len);
                }
            }
            catch (std::bad_alloc &e)
            {
                e.what();
                ptr = nullptr;
                throw Exception(ERROR_MEMORY_ALLOCATION);
            }
        }
        return ptr;
    }

}

