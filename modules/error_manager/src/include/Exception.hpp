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
#include <cstring>
#include <vector>
#include "nlsError_manager_exports.h"
#include "Interface.hpp"
#include "PositionScript.hpp"
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
    //=============================================================================
private:
    std::vector<PositionScript> backtrace;
    std::vector<Exception> cause;
    std::wstring identifier = L"";
    std::wstring msg = L"";
    //=============================================================================
public:
    Exception(const std::string& msg_in, std::vector<PositionScript> positions,
        const std::string& identifier_in = "");
    //=============================================================================
    Exception(std::wstring msg_in, std::vector<PositionScript> positions,
        std::wstring identifier_in = L"");
    //=============================================================================
    Exception(const std::string& msg_in, const PositionScript& position,
        const std::string& identifier_in = "");
    //=============================================================================
    Exception(const std::wstring& msg_in, const PositionScript& position,
        const std::wstring& identifier_in = L"");
    //=============================================================================
    Exception(const std::string& msg_in, const std::string& identifier_in = "");
    //=============================================================================
    Exception(const std::wstring& msg_in, const std::wstring& identifier_in = L"");
    //=============================================================================
    Exception();
    //=============================================================================
    /**
     * Copy constructor.
     */
    Exception(const Exception& copy);
    //=============================================================================
    /**
     * Assignment operator.
     */
    void
    operator=(const Exception& copy);
    //=============================================================================
    /**
     * Standard destructor.
     */
    ~Exception();
    //=============================================================================
    /**
     * Output the contents of the exception to the console.
     */
    void
    printMe(Interface* io) const;
    //=============================================================================
    /**
     * compares messages
     */
    bool
    matches(const std::string& tst_msg);
    //=============================================================================
    bool
    matches(const std::wstring& tst_msg);
    //=============================================================================
    /**
     * Get the message member function.
     */
    [[nodiscard]] std::wstring
    getMessage() const
    {
        return msg;
    }
    //=============================================================================
    void
    setMessage(const std::wstring& message_in);
    //=============================================================================
    void
    setMessage(const std::string& message_in);
    //=============================================================================
    void
    setIdentifier(const std::wstring& identifier_in);
    //=============================================================================
    void
    setIdentifier(const std::string& identifier_in);
    //=============================================================================
    [[nodiscard]] std::vector<Exception>
    getCause() const
    {
        return cause;
    }
    //=============================================================================
    void
    setCause(const std::vector<Exception>& _cause)
    {
        cause = _cause;
    }
    //=============================================================================
    [[nodiscard]] std::wstring
    getFormattedErrorMessage() const;
    //=============================================================================
    [[nodiscard]] std::wstring
    what() const
    {
        return msg;
    }
    //=============================================================================
    [[nodiscard]] std::wstring
    getFilename() const;
    //=============================================================================
    [[nodiscard]] int
    getLine() const;
    //=============================================================================
    [[nodiscard]] std::wstring
    getFunctionName() const;
    //=============================================================================
    [[nodiscard]] bool
    isEmpty() const;
    //=============================================================================
    [[nodiscard]] std::wstring
    getIdentifier() const
    {
        return identifier;
    }
    //=============================================================================
    [[nodiscard]] std::vector<PositionScript>
    getTrace() const
    {
        return backtrace;
    }
    //=============================================================================
    void
    setTrace(const std::vector<PositionScript>& _trace)
    {
        backtrace = _trace;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
