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

#include "Diary.hpp"
#include "nlsStream_manager_exports.h"
#include <string>

namespace Nelson {
class NLSSTREAM_MANAGER_IMPEXP Interface
{
public:
    Diary diary;
    Interface();
    virtual ~Interface() = 0;
    /**
     *  Get a line of input from the user with the
     *  given prompt.
     */
    virtual std::string
    getLine(std::string prompt)
        = 0;
    virtual std::wstring
    getLine(std::wstring prompt)
        = 0;
    virtual std::wstring
    getInput(std::wstring prompt)
        = 0;

    /**
     *  Return the width of the current "terminal" in
     *  characters.
     */
    virtual size_t
    getTerminalWidth()
        = 0;
    /**
     *  Output the following text message.
     */
    virtual void
    outputMessage(std::string msg)
        = 0;
    virtual void
    outputMessage(std::wstring msg)
        = 0;
    /**
     *  Output the following error message.
     */
    virtual void
    errorMessage(std::string msg)
        = 0;
    virtual void
    errorMessage(std::wstring msg)
        = 0;
    /**
     *  Output the following warning message.
     */
    virtual void
    warningMessage(std::string msg)
        = 0;
    virtual void
    warningMessage(std::wstring msg)
        = 0;

    virtual void
    clearTerminal()
        = 0;
    virtual bool
    isAtPrompt()
        = 0;
};
} // namespace Nelson
