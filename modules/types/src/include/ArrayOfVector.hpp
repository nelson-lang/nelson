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
#include <vector>
#include <iterator>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
class ArrayOf;
//=============================================================================
template <typename T> class ListVector
{
private:
    std::vector<T> vector;
    //=============================================================================
public:
    //=============================================================================
    inline ListVector() = default;
    //=============================================================================
    inline ListVector(size_t r) { vector.reserve(r); }
    //=============================================================================
    inline ListVector(const T& val) { vector.push_back(val); }
    //=============================================================================
    inline ListVector(const ListVector& copy) : vector(copy.vector) { }
    //=============================================================================
    inline ~ListVector() = default;
    //=============================================================================
    inline void
    reserve(size_t r)
    {
        vector.reserve(r);
    }
    //=============================================================================
    inline void
    resize(size_t r)
    {
        vector.resize(r);
    }
    //=============================================================================
    inline void
    clear()
    {
        vector.clear();
    }
    //=============================================================================
    [[nodiscard]] inline size_t
    size() const
    {
        return vector.size();
    }
    //=============================================================================
    [[nodiscard]] inline bool
    empty() const
    {
        return vector.empty();
    }
    //=============================================================================
    inline ListVector&
    operator=(const ListVector& copy)
    {
        if (&copy == this) {
            return *this;
        }
        vector.reserve(copy.vector.size());
        vector = copy.vector;
        return *this;
    }
    //=============================================================================
    inline void
    push_back(const T& el)
    {
        vector.push_back(el);
    }
    //=============================================================================
    inline void
    push_front(const T& value)
    {
        vector.insert(vector.begin(), value);
    }
    //=============================================================================
    inline void
    pop_front()
    {
        vector.erase(vector.begin());
    }
    //=============================================================================
    inline void
    pop_back()
    {
        vector.pop_back();
    }
    //=============================================================================
    inline T&
    front()
    {
        return vector.front();
    }
    //=============================================================================
    [[nodiscard]] inline const T&
    front() const
    {
        return vector.front();
    }
    //=============================================================================
    inline T&
    back()
    {
        return vector.back();
    }
    //=============================================================================
    [[nodiscard]] inline const T&
    back() const
    {
        return vector.back();
    }
    //=============================================================================
    inline T&
    operator[](size_t i)
    {
        return vector[i];
    }
    //=============================================================================
    inline const T&
    operator[](size_t i) const
    {
        return vector[i];
    }
    //=============================================================================
    [[nodiscard]] inline const T&
    at(size_t i) const
    {
        return vector.at(i);
    }
    //=============================================================================
    inline ListVector&
    operator<<(const T& other)
    {
        push_back(other);
        return *this;
    }
    //=============================================================================
    inline ListVector&
    operator+=(const ListVector<T>& other)
    {
        vector.reserve(other.size() + vector.size());
        for (size_t i = 0; i < other.size(); i++) {
            push_back(other.at(i));
        }
        return *this;
    }
    //=============================================================================
    inline typename std::vector<T>::iterator
    begin() noexcept
    {
        return vector.begin();
    }
    //=============================================================================
    [[nodiscard]] inline typename std::vector<T>::const_iterator
    begin() const noexcept
    {
        return vector.begin();
    }
    //=============================================================================
    inline typename std::vector<T>::iterator
    end() noexcept
    {
        return vector.end();
    }
    //=============================================================================
    [[nodiscard]] inline typename std::vector<T>::const_iterator
    end() const noexcept
    {
        return vector.end();
    }
    //=============================================================================
};
//=============================================================================
using ArrayOfVector = ListVector<ArrayOf>;
//=============================================================================
using ArrayOfMatrix = std::vector<ArrayOfVector>;
//=============================================================================
}
//=============================================================================
