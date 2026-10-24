//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOfView.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Creating the view asks ArrayOf for writable storage, so copy-on-write is
// already done before data() is exposed.
template <typename T> class MutableArrayOfView
{
private:
    T* data_ { nullptr };
    Dimensions dimensions_;
    indexType elementCount_ { 0 };

    void
    bind(ArrayOf& array)
    {
        data_ = static_cast<T*>(array.readWriteDataPointer());
        dimensions_ = array.dimensions();
        elementCount_ = array.elementCount();
    }

public:
    MutableArrayOfView(ArrayOf& array, NelsonType expectedClass)
    {
        if (array.isSparse()) {
            Error(L"Nelson:types:sparseNotSupported:operationDoesNotSupportSparseMatrix",
                L"operation does not support sparse matrix arguments.");
        }
        if (array.dataClass() != expectedClass) {
            Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
        }
        bind(array);
    }

    // The caller has already validated the array.
    MutableArrayOfView(ArrayOf& array, ArrayOfViewAssumeValidated) { bind(array); }

    [[nodiscard]] T*
    data() const
    {
        return data_;
    }

    [[nodiscard]] T&
    operator[](indexType index) const
    {
        return data_[index];
    }

    [[nodiscard]] const Dimensions&
    dimensions() const
    {
        return dimensions_;
    }

    [[nodiscard]] indexType
    elementCount() const
    {
        return elementCount_;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
