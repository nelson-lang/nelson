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
#include <cstddef>
#include <new>
#include <type_traits>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
class ArrayOf;
//=============================================================================
// Argument and result lists are built and freed on every call and rarely hold more than a few
// values: their buffers are recycled per thread. Blocks come from operator new, so any code may
// free them, and the allocator is stateless, so a list keeps the std::vector layout.
template <typename T> class ListBufferAllocator
{
public:
    using value_type = T;
    using is_always_equal = std::true_type;
    ListBufferAllocator() noexcept = default;
    template <typename U> ListBufferAllocator(const ListBufferAllocator<U>&) noexcept { }
    //=============================================================================
    T*
    allocate(std::size_t n)
    {
        if (n >= 1 && n <= recycledSizes) {
            Slot& slot = threadCache().lists[n - 1];
            if (slot.count > 0) {
                return static_cast<T*>(slot.blocks[--slot.count]);
            }
        }
        return static_cast<T*>(::operator new(n * sizeof(T)));
    }
    //=============================================================================
    void
    deallocate(T* block, std::size_t n) noexcept
    {
        if (n >= 1 && n <= recycledSizes) {
            Slot& slot = threadCache().lists[n - 1];
            if (slot.count < blocksPerSize) {
                slot.blocks[slot.count++] = block;
                return;
            }
        }
        ::operator delete(block);
    }
    //=============================================================================
    template <typename U>
    bool
    operator==(const ListBufferAllocator<U>&) const noexcept
    {
        return true;
    }
    //=============================================================================
    template <typename U>
    bool
    operator!=(const ListBufferAllocator<U>&) const noexcept
    {
        return false;
    }
    //=============================================================================
private:
    static constexpr std::size_t recycledSizes = 4;
    static constexpr std::size_t blocksPerSize = 16;
    struct Slot
    {
        void* blocks[blocksPerSize];
        std::size_t count = 0;
    };
    struct Cache
    {
        Slot lists[recycledSizes];
        ~Cache()
        {
            for (Slot& slot : lists) {
                while (slot.count > 0) {
                    ::operator delete(slot.blocks[--slot.count]);
                }
            }
        }
    };
    static Cache&
    threadCache()
    {
        static thread_local Cache cache;
        return cache;
    }
};
//=============================================================================
/**
 * Small vector wrapper used for Nelson argument and result lists.
 *
 * The wrapper keeps the
 * old stream-like append operator while exposing the
 * subset of std::vector operations used
 * throughout builtins.
 */
template <typename T> class ListVector
{
private:
    using Storage = std::vector<T, ListBufferAllocator<T>>;
    Storage vector;
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
    // the user-declared copy constructor suppresses the implicit move one
    inline ListVector(ListVector&& other) noexcept : vector(std::move(other.vector)) { }
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
    inline void
    push_back(const T& el)
    {
        vector.push_back(el);
    }
    //=============================================================================
    inline void
    push_back(T&& el)
    {
        vector.push_back(std::move(el));
    }
    //=============================================================================
    template <typename... Args>
    inline T&
    emplace_back(Args&&... args)
    {
        return vector.emplace_back(std::forward<Args>(args)...);
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
    operator=(const ListVector& copy)
    {
        if (&copy == this) {
            return *this;
        }
        if (vector.capacity() < copy.vector.size()) {
            vector.reserve(copy.vector.size());
        }
        vector = copy.vector;
        return *this;
    }
    //=============================================================================
    inline ListVector&
    operator=(ListVector&& other) noexcept
    {
        if (this != &other) {
            vector = std::move(other.vector);
        }
        return *this;
    }
    //=============================================================================
    inline ListVector&
    operator+=(const ListVector<T>& other)
    {
        if (vector.capacity() < other.size() + vector.size()) {
            vector.reserve(other.size() + vector.size());
        }
        std::copy(other.begin(), other.end(), std::back_inserter(vector));
        return *this;
    }
    //=============================================================================
    inline typename Storage::iterator
    begin() noexcept
    {
        return vector.begin();
    }
    //=============================================================================
    [[nodiscard]] inline typename Storage::const_iterator
    begin() const noexcept
    {
        return vector.begin();
    }
    //=============================================================================
    inline typename Storage::iterator
    end() noexcept
    {
        return vector.end();
    }
    //=============================================================================
    [[nodiscard]] inline typename Storage::const_iterator
    end() const noexcept
    {
        return vector.end();
    }
    //=============================================================================
};
//=============================================================================
/**
 * Ordered list of ArrayOf values, usually function inputs or outputs.
 */
using ArrayOfVector = ListVector<ArrayOf>;
//=============================================================================
/**
 * Two-dimensional list of ArrayOf values.
 */
using ArrayOfMatrix = std::vector<ArrayOfVector>;
//=============================================================================
} // namespace Nelson
//=============================================================================
