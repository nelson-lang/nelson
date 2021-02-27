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
#pragma once
//=============================================================================
#include <string>
#include <vector>
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
    pushEntry(const std::string& _context, const std::string& _detail, size_t _id)
    {
        if (IDX >= context.size()) {
            context.push_back(_context);
            detail.push_back(_detail);
            id.push_back(_id);

        } else {
            context[IDX] = _context;
            detail[IDX] = _detail;
            id[IDX] = _id;
        }
        IDX++;
    }
    //=============================================================================
public:
    CallStack() {};
    //=============================================================================
    ~CallStack() { clear(); }
    //=============================================================================
    void
    clear()
    {
        context.clear();
        detail.clear();
        id.clear();
        context.reserve(4096);
        detail.reserve(4096);
        id.reserve(4096);
        IDX = 0;
    }
    //=============================================================================
    size_t
    size() const
    {
        return IDX;
    }
    //=============================================================================
    std::string
    getContext(size_t pos) const
    {
        return context[pos];
    }
    //=============================================================================
    std::string
    getDetail(size_t pos) const
    {
        return detail[pos];
    }
    //=============================================================================
    size_t
    getID(size_t pos) const
    {
        return id[pos];
    }
    //=============================================================================
    std::string
    getLastContext() const
    {
        return context[IDX - 1];
    }
    //=============================================================================
    std::string
    getLastDetail()
    {
        return detail[IDX - 1];
    }
    //=============================================================================
    size_t
    getLastID()
    {
        return id[IDX - 1];
    }
    //=============================================================================
    void
    pushDebug(const std::string& _name, const std::string& _detail)
    {
        pushEntry(_name, _detail, 0);
    }
    //=============================================================================
    void
    popDebug()
    {
        pop_back();
    }
    //=============================================================================
    void
    pushID(size_t _id)
    {
        if (IDX == 0) {
            pushEntry("base", "base", _id);
        } else {
            pushEntry(context[IDX - 1], detail[IDX - 1], _id);
        }
    }
    //=============================================================================
    void
    popID()
    {
        pop_back();
    }
    //=============================================================================
    void
    pop_back()
    {
        if (IDX > 0) {
            IDX--;
        };
    }
    //=============================================================================
};
//=============================================================================

}
//=============================================================================
