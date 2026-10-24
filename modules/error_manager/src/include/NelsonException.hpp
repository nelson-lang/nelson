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
#include <cstring>
#include <memory>
#include <utility>
#include <vector>
#include "nlsError_manager_exports.h"
#include "Interface.hpp"
#include "PositionScript.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class ArrayOf;
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
class NLSERROR_MANAGER_IMPEXP NelsonException
{
    //=============================================================================
private:
    std::vector<PositionScript> backtrace;
    std::vector<NelsonException> cause_;
    std::shared_ptr<ArrayOf> correction;
    std::wstring identifier_ = L"";
    std::wstring msg = L"";
    //=============================================================================
public:
    NelsonException(const std::string& msg_in, std::vector<PositionScript> positions,
        const std::string& identifier_in = "");
    //=============================================================================
    NelsonException(std::wstring msg_in, std::vector<PositionScript> positions,
        std::wstring identifier_in = L"");
    //=============================================================================
    NelsonException(const std::string& msg_in, const PositionScript& position,
        const std::string& identifier_in = "");
    //=============================================================================
    NelsonException(const std::wstring& msg_in, const PositionScript& position,
        const std::wstring& identifier_in = L"");
    //=============================================================================
    NelsonException(const std::string& msg_in, const std::string& identifier_in = "");
    //=============================================================================
    NelsonException(const std::wstring& msg_in, const std::wstring& identifier_in = L"");
    //=============================================================================
    NelsonException();
    //=============================================================================
    NelsonException(const NelsonException& copy);
    //=============================================================================
    NelsonException(NelsonException&& move) noexcept;
    //=============================================================================
    NelsonException&
    operator=(const NelsonException& copy);
    //=============================================================================
    NelsonException&
    operator=(NelsonException&& move) noexcept;
    //=============================================================================
    ~NelsonException();
    //=============================================================================
    void
    printMe(Interface* io) const;
    //=============================================================================
    bool
    matches(const std::string& tst_msg);
    //=============================================================================
    bool
    matches(const std::wstring& tst_msg);
    //=============================================================================
    [[nodiscard]] const std::wstring&
    message() const
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
    [[nodiscard]] const std::vector<NelsonException>&
    cause() const
    {
        return cause_;
    }
    //=============================================================================
    void
    setCause(std::vector<NelsonException> _cause)
    {
        cause_ = std::move(_cause);
    }
    //=============================================================================
    [[nodiscard]] const std::shared_ptr<ArrayOf>&
    correctionHandle() const
    {
        return correction;
    }
    //=============================================================================
    void
    setCorrectionHandle(std::shared_ptr<ArrayOf> _correction)
    {
        correction = std::move(_correction);
    }
    //=============================================================================
    [[nodiscard]] bool
    hasCorrection() const
    {
        return correction != nullptr;
    }
    //=============================================================================
    [[nodiscard]] std::wstring
    formattedErrorMessage(bool withHyperlinks = false) const;
    //=============================================================================
    // Warning text for the command window. With withHyperlinks the top user frame
    // is appended as a clickable location link; without it the bare message is
    // returned unchanged (plain terminals keep their current output).
    [[nodiscard]] std::wstring
    formattedWarningMessage(bool withHyperlinks = false) const;
    //=============================================================================
    [[nodiscard]] const std::wstring&
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
    [[nodiscard]] const std::wstring&
    identifier() const
    {
        return identifier_;
    }
    //=============================================================================
    [[nodiscard]] const std::vector<PositionScript>&
    trace() const
    {
        return backtrace;
    }
    //=============================================================================
    void
    setTrace(std::vector<PositionScript> _trace)
    {
        backtrace = std::move(_trace);
    }
    //=============================================================================
    void
    clear();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
