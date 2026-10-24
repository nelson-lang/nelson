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
#include <stdexcept>
#include <utility>
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /// Item stored in the A* open set.
    struct HeapItem
    {
        int idx = 0;
        double f = 0.0;
        std::uint64_t seq = 0; ///< insertion order, breaks ties on equal f
    };
    //=============================================================================
    /// Min-heap ordered by (HeapItem::f, HeapItem::seq). The insertion-order
    /// tie-break makes the pop order deterministic and identical to the TS router.
    class RouterMinHeap
    {
    public:
        RouterMinHeap() = default;

        bool
        empty() const
        {
            return items_.empty();
        }
        std::size_t
        size() const
        {
            return items_.size();
        }

        void
        push(int idx, double f)
        {
            items_.push_back({ idx, f, counter_++ });
            bubbleUp(static_cast<int>(items_.size()) - 1);
        }

        HeapItem
        pop()
        {
            if (items_.empty()) {
                throw std::runtime_error("pop from empty heap");
            }
            if (items_.size() == 1) {
                auto t = items_[0];
                items_.clear();
                return t;
            }
            auto top = items_[0];
            items_[0] = items_.back();
            items_.pop_back();
            bubbleDown(0);
            return top;
        }

    private:
        std::vector<HeapItem> items_;
        std::uint64_t counter_ = 0;

        static bool
        less(const HeapItem& a, const HeapItem& b)
        {
            if (a.f != b.f) {
                return a.f < b.f;
            }
            return a.seq < b.seq;
        }

        void
        bubbleUp(int idx)
        {
            while (idx > 0) {
                int parent = (idx - 1) / 2;
                if (!less(items_[idx], items_[parent])) {
                    break;
                }
                std::swap(items_[parent], items_[idx]);
                idx = parent;
            }
        }

        void
        bubbleDown(int idx)
        {
            int n = static_cast<int>(items_.size());
            while (true) {
                int left = idx * 2 + 1;
                int right = idx * 2 + 2;
                int smallest = idx;
                if (left < n && less(items_[left], items_[smallest])) {
                    smallest = left;
                }
                if (right < n && less(items_[right], items_[smallest])) {
                    smallest = right;
                }
                if (smallest == idx) {
                    break;
                }
                std::swap(items_[smallest], items_[idx]);
                idx = smallest;
            }
        }
    };
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
