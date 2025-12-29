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
    size_t IDX = 0;
    //=============================================================================
    void
    pushEntry(std::string_view _context, std::string_view _detail, size_t _id) noexcept
    {
        if (IDX >= context.size()) {
            context.emplace_back(_context);
            detail.emplace_back(_detail);
            id.emplace_back(_id);
        } else {
            context[IDX] = std::move(_context);
            detail[IDX] = std::move(_detail);
            id[IDX] = _id;
        }
        IDX++;
    }
    //=============================================================================
public:
    CallStack()
    {
        context.reserve(2048);
        detail.reserve(2048);
        id.reserve(2048);
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
        return context[pos];
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getDetail(size_t pos) const noexcept
    {
        return detail[pos];
    }
    //=============================================================================
    [[nodiscard]] size_t
    getID(size_t pos) const noexcept
    {
        return id[pos];
    }
    //=============================================================================
    [[nodiscard]] const std::string&
    getLastContext() const noexcept
    {
        return context[IDX - 1];
    }
    //=============================================================================
    const std::string&
    getLastDetail() const noexcept
    {
        return detail[IDX - 1];
    }
    //=============================================================================
    size_t
    getLastID() const noexcept
    {
        return id[IDX - 1];
    }
    //=============================================================================
    void
    pushDebug(std::string_view _name, std::string_view _detail)
    {
        pushEntry(_name, _detail, 0);
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
        if (IDX == 0) {
            pushEntry(std::string_view("base"), std::string_view("base"), _id);
        } else {
            pushEntry(context[IDX - 1], detail[IDX - 1], _id);
        }
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

}
//=============================================================================
