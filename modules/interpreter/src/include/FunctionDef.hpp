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
//=============================================================================
#pragma once
//=============================================================================
#include <time.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    NLS_MACRO_FUNCTION,
    NLS_BUILT_IN_FUNCTION,
    NLS_MEX_FUNCTION,
} FunctionType;
//=============================================================================
class Evaluator;
//=============================================================================
/** Base class for the function types
 * A FunctionDef class is a base class for the different types
 * of function pointers used.  There are three types of functions
 * available:
 *    - M-functions - these are functions or scripts written by the
 *      user in the interpreted language.
 *    - Built-in functions - these are functions coded in C++ that
 *      implement functionality too difficult/impossible to do in
 *      the language itself.
 *    - MEX functions - compatible MEX functions
 * All of these functions have in common a name, a script classification
 * (is it a script or not), a well defined number of input arguments,
 * a well defined number of output arguments, and some means of
 * being evaluated.
 */
class NLSINTERPRETER_IMPEXP FunctionDef
{
private:
    //=============================================================================
    /**
     * The name of the function - must follow identifier rules.
     */
    std::string name;
    //=============================================================================
    /**
     * The filename of the function.
     */
    std::wstring filename;
    //=============================================================================
    std::wstring pathname;
    //=============================================================================
    time_t timestamp;
    //=============================================================================
public:
    //=============================================================================
    void
    setFilename(const std::wstring& filename)
    {
        this->filename = filename;
        boost::filesystem::path path(filename);
        this->pathname = path.parent_path().generic_wstring();
        this->timestamp = boost::filesystem::last_write_time(filename);
    }
    //=============================================================================
    std::wstring
    getFilename()
    {
        return this->filename;
    }
    //=============================================================================
    std::wstring
    getPath()
    {
        return this->pathname;
    }
    //=============================================================================
    void
    setName(const std::string& name)
    {
        this->name = name;
    }
    //=============================================================================
    std::string
    getName()
    {
        return this->name;
    }
    //=============================================================================
    /**
     * The names of the arguments to the fuction (analogous to returnVals).
     * Should have "varargin" as the last entry for variable argument
     * functions.
     */
    stringVector arguments;
    //=============================================================================
    /**
     * The constructor.
     */
    FunctionDef();
    //=============================================================================
    /**
     * The virtual destructor
     */
    virtual ~FunctionDef();
    //=============================================================================
    /**
     * The type of the function (NLS_MACRO_FUNCTION, NLS_BUILT_IN_FUNCTION).
     */
    virtual FunctionType
    type() const = 0;
    //=============================================================================
    /**
     * The number of inputs required by this function (-1 if variable).
     */
    virtual int
    inputArgCount()
        = 0;
    //=============================================================================
    /**
     * The number of outputs returned by this function (-1 if variable).
     */
    virtual int
    outputArgCount()
        = 0;
    //=============================================================================
    /**
     * Evaluate the function and return its output.
     */
    virtual ArrayOfVector
    evaluateFunction(Evaluator*, const ArrayOfVector&, int)
        = 0;
    //=============================================================================
    virtual bool
    updateCode()
        = 0;
    //=============================================================================
    void
    setTimestamp(time_t timestamp)
    {
        this->timestamp = timestamp;
    }
    //=============================================================================
    time_t
    getTimestamp()
    {
        return this->timestamp;
    }
    //=============================================================================
};
//=============================================================================
using FunctionDefPtr = FunctionDef*;
//=============================================================================
} // namespace Nelson
//=============================================================================
