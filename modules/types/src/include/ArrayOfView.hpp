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
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ArrayOfViewAssumeValidated
{ };
//=============================================================================
/**
 * Read-only typed view over dense ArrayOf storage.
 *
 * The view does not own the data. It validates the expected Nelson class,
 * rejects sparse arrays, and keeps a copy of dimensions and element count for
 * fast loop setup.
 */
template <typename T> class ArrayOfView
{
private:
    const T* data_ { nullptr };
    Dimensions dimensions_;
    indexType elementCount_ { 0 };

    void
    bind(const ArrayOf& array)
    {
        data_ = static_cast<const T*>(array.dataPointer());
        dimensions_ = array.dimensions();
        elementCount_ = array.elementCount();
    }

public:
    /**
     * Create a typed view after checking class and sparsity.
     */
    ArrayOfView(const ArrayOf& array, NelsonType expectedClass)
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

    ArrayOfView(const ArrayOf& array, ArrayOfViewAssumeValidated) { bind(array); }

    /**
     * Return the first element of the viewed storage.
     */
    [[nodiscard]] const T*
    data() const
    {
        return data_;
    }

    /**
     * Return an element by linear index.
     */
    [[nodiscard]] const T&
    operator[](indexType index) const
    {
        return data_[index];
    }

    /**
     * Return the dimensions captured when the view was created.
     */
    [[nodiscard]] const Dimensions&
    dimensions() const
    {
        return dimensions_;
    }

    /**
     * Return the element count captured when the view was created.
     */
    [[nodiscard]] indexType
    elementCount() const
    {
        return elementCount_;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
