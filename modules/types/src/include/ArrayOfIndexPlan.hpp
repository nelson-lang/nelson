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
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Prepared representation of an ArrayOf indexing expression.
 *
 * The plan owns any expanded colon indexes and keeps direct pointers to
 * already-materialized ordinal indexes. It also records the output dimensions
 * and the first preserved colon, when the caller needs to keep one dimension.
 */
class IndexPlan
{
private:
    std::vector<constIndexPtr> pointers_;
    std::vector<std::vector<indexType>> ownedIndexes_;
    Dimensions outDims_;
    bool anyEmpty_ { false };
    indexType colonIndex_ { static_cast<indexType>(-1) };

public:
    /**
     * Build an index plan for dims and index arguments.
     *
     * Non-colon index arrays are converted to ordinal form in place. Bounds are
     * checked when checkBounds is true.
     */
    IndexPlan(bool preserveColons, Dimensions& dims, ArrayOfVector& index, bool checkBounds)
        : pointers_(index.size(), nullptr), ownedIndexes_(index.size()), outDims_(index.size())
    {
        bool colonFound = false;
        for (size_t i = 0; i < index.size(); i++) {
            bool isColon = isColonOperator(index[i]);
            if (!colonFound && isColon && preserveColons) {
                colonFound = true;
                colonIndex_ = i;
                outDims_[i] = dims[i];
            } else if (isColon) {
                ownedIndexes_[i].resize(dims[i]);
                for (indexType j = 0; j < dims[i]; j++) {
                    ownedIndexes_[i][j] = j + 1;
                }
                pointers_[i] = ownedIndexes_[i].data();
                outDims_[i] = dims[i];
            } else if (index[i].isEmpty()) {
                anyEmpty_ = true;
                outDims_[i] = 0;
            } else {
                index[i].toOrdinalType();
                if (checkBounds && index[i].maxAsIndex() > dims[i]) {
                    Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                }
                pointers_[i] = static_cast<constIndexPtr>(index[i].dataPointer());
                outDims_[i] = index[i].elementCount();
            }
        }
    }

    /**
     * Return the per-dimension index pointer array.
     */
    [[nodiscard]] constIndexPtr*
    data()
    {
        return pointers_.data();
    }

    /**
     * Return whether any index argument is empty.
     */
    [[nodiscard]] bool
    anyEmpty() const
    {
        return anyEmpty_;
    }

    /**
     * Return the preserved colon dimension, or -1 cast to indexType.
     */
    [[nodiscard]] indexType
    colonIndex() const
    {
        return colonIndex_;
    }

    /**
     * Return the dimensions produced by the indexing expression.
     */
    [[nodiscard]] const Dimensions&
    outDims() const
    {
        return outDims_;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
