//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include <cassert>
#include <cstdint>
#include <string_view>
//=============================================================================
namespace Nelson {
//=============================================================================
class CallStack
{
private:
    //=============================================================================
    std::vector<std::string> context;
    std::vector<std::string> detail;
    std::vector<size_t> id;
    std::vector<uint8_t> directScript;
    size_t IDX = 0;
    //=============================================================================
    void
    pushEntry(std::string_view _context, std::string_view _detail, size_t _id, bool _directScript)
    {
        if (IDX >= context.size()) {
            context.emplace_back(_context);
            detail.emplace_back(_detail);
            id.emplace_back(_id);
            directScript.emplace_back(static_cast<uint8_t>(_directScript ? 1 : 0));
        } else {
            context[IDX].assign(_context);
            detail[IDX].assign(_detail);
            id[IDX] = _id;
            directScript[IDX] = _directScript ? 1 : 0;
        }
        IDX++;
    }
    //=============================================================================
    // Index of the debug entry (id == 0) governing position pos: pos itself
    // when it is a debug entry, else the nearest debug entry below. ID entries
    // (statement markers) carry no strings of their own; their context/detail
    // are those of the enclosing debug entry. Returns npos when there is none
    // (ID entries pushed on an empty stack).
    [[nodiscard]] size_t
    ownerEntry(size_t pos) const noexcept
    {
        size_t k = pos;
        while (k > 0 && id[k] != 0) {
            --k;
        }
        return (id[k] == 0) ? k : static_cast<size_t>(-1);
    }
    //=============================================================================
    [[nodiscard]] static const std::string&
    baseEntryString() noexcept
    {
        static const std::string base { "base" };
        return base;
    }
    //=============================================================================
public:
    CallStack()
    {
        context.reserve(2048);
        detail.reserve(2048);
        id.reserve(2048);
        directScript.reserve(2048);
    }
    //=============================================================================
    CallStack(const CallStack& other)
        : context(other.context)
        , detail(other.detail)
        , id(other.id)
        , directScript(other.directScript)
        , IDX(other.IDX)
    {
    }
    //=============================================================================
    CallStack(CallStack&& other) noexcept
        : context(std::move(other.context))
        , detail(std::move(other.detail))
        , id(std::move(other.id))
        , directScript(std::move(other.directScript))
        , IDX(other.IDX)
    {
        other.IDX = 0;
    }
    //=============================================================================
    CallStack&
    operator=(const CallStack& other)
    {
        if (this != &other) {
            context = other.context;
            detail = other.detail;
            id = other.id;
            directScript = other.directScript;
            IDX = other.IDX;
        }
        return *this;
    }
    //=============================================================================
    CallStack&
    operator=(CallStack&& other) noexcept
    {
        if (this != &other) {
            context = std::move(other.context);
            detail = std::move(other.detail);
            id = std::move(other.id);
            directScript = std::move(other.directScript);
            IDX = other.IDX;
            other.IDX = 0;
        }
        return *this;
    }
    //=============================================================================
    ~CallStack() = default;
    //=============================================================================
    void
    clear() noexcept
    {
        context.clear();
        detail.clear();
        id.clear();
        directScript.clear();
        IDX = 0;
    }
    //=============================================================================
    [[nodiscard]] size_t
    size() const noexcept
    {
        return IDX;
    }
    //=============================================================================
    void
    setSize(size_t newSize) noexcept
    {
        if (newSize < IDX) {
            IDX = newSize;
        }
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getContext(size_t pos) const noexcept
    {
        size_t k = ownerEntry(pos);
        return (k == static_cast<size_t>(-1)) ? baseEntryString() : context[k];
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getDetail(size_t pos) const noexcept
    {
        size_t k = ownerEntry(pos);
        return (k == static_cast<size_t>(-1)) ? baseEntryString() : detail[k];
    }
    //=============================================================================
    [[nodiscard]] size_t
    getID(size_t pos) const noexcept
    {
        return id[pos];
    }
    //=============================================================================
    [[nodiscard]] bool
    isDirectScript(size_t pos) const noexcept
    {
        size_t k = ownerEntry(pos);
        return k != static_cast<size_t>(-1) && directScript[k] != 0;
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getLastContext() const noexcept
    {
        static const std::string empty {};
        if (IDX > 0) {
            return getContext(IDX - 1);
        }
        return empty;
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getLastDetail() const noexcept
    {
        static const std::string empty {};
        if (IDX > 0) {
            return getDetail(IDX - 1);
        }
        return empty;
    }
    //=============================================================================
    [[nodiscard]] size_t
    getLastID() const noexcept
    {
        return (IDX > 0) ? id[IDX - 1] : 0;
    }
    //=============================================================================
    void
    pushDebug(std::string_view _name, std::string_view _detail, bool _directScript = false)
    {
        pushEntry(_name, _detail, 0, _directScript);
    }
    //=============================================================================
    void
    popDebug() noexcept
    {
        pop_back();
    }
    //=============================================================================
    void
    pushID(size_t _id)
    {
        // ID entries carry no strings: getContext/getDetail resolve them to
        // the enclosing debug entry (or "base"). Recycled slots may keep stale
        // text, which is never read.
        if (IDX >= context.size()) {
            context.emplace_back();
            detail.emplace_back();
            id.emplace_back(_id);
            directScript.emplace_back(static_cast<uint8_t>(0));
        } else {
            id[IDX] = _id;
            directScript[IDX] = 0;
        }
        IDX++;
    }
    //=============================================================================
    void
    popID() noexcept
    {
        pop_back();
    }
    //=============================================================================
    void
    pop_back() noexcept
    {
        if (IDX > 0) {
            IDX--;
        }
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
