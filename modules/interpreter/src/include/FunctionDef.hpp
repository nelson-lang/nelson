//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <mutex>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum FunctionType
{
    NLS_MACRO_FUNCTION = 0,
    NLS_ANONYMOUS_MACRO_FUNCTION,
    NLS_BUILT_IN_FUNCTION,
    NLS_MEX_FUNCTION,
};
//=============================================================================
enum FunctionOverloadAutoMode
{
    NLS_OVERLOAD_AUTO_ON = 0,
    NLS_OVERLOAD_AUTO_OFF,
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
    bool _isOverload;
    //=============================================================================
    /**
     * The filename of the function.
     */
    std::wstring filename;
    //=============================================================================
    std::wstring pathname;
    //=============================================================================
    std::mutex m_mutex;
    //=============================================================================
public:
    //=============================================================================
    FunctionOverloadAutoMode overloadAutoMode = NLS_OVERLOAD_AUTO_ON;
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
    bool
    isOverload()
    {
        return this->_isOverload;
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
    FunctionDef(bool isOverload);
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
    evaluateFunction(Evaluator*, const ArrayOfVector&, int)
        = 0;
    //=============================================================================
    virtual bool
    updateCode()
        = 0;
    //=============================================================================
};
//=============================================================================
using FunctionDefPtr = FunctionDef*;
//=============================================================================
} // namespace Nelson
//=============================================================================
