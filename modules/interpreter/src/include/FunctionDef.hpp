//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <ctime>
#include <mutex>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum FunctionType
{
    NLS_MACRO_FUNCTION,
    NLS_ANONYMOUS_MACRO_FUNCTION,
    NLS_BUILT_IN_FUNCTION,
    NLS_MEX_FUNCTION,
};
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
    std::mutex m_mutex;
    //=============================================================================
public:
    //=============================================================================
    void
    lock()
    {
        std::scoped_lock<std::mutex> _lock { m_mutex };
    }
    //=============================================================================
    void
    setFilename(const std::wstring& filename);
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
    [[nodiscard]] virtual FunctionType
    type() const
        = 0;
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
    evaluateFunction(Evaluator*, const ArrayOfVector&, int) //-V1071
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
