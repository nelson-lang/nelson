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
#include "GOFixedVectorProperty.hpp"
#include <cstdint>
//=============================================================================
namespace Nelson {
class GraphicsObject;
//=============================================================================
class GOFourVectorProperty : public GOFixedVectorProperty
{
public:
    GOFourVectorProperty() : GOFixedVectorProperty(4) { }
    ~GOFourVectorProperty() override = default;
    void
    value(double x, double y, double z, double w);
    std::wstring
    toWideString() override;
    void set(ArrayOf) override;
    // Rank of the last set, compared with the Units rank to know the given order.
    uint64_t
    getSetOrder();

private:
    uint64_t m_setOrder = 0;
};
//=============================================================================
// Position kept zero-based in pixels, seen one-based through get and set.
class GOPixelOriginPositionProperty : public GOFourVectorProperty
{
public:
    explicit GOPixelOriginPositionProperty(GraphicsObject* owner) : m_owner(owner) { }
    ~GOPixelOriginPositionProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    std::wstring
    toWideString() override;
    // Re-applies the origin when Units changed after the value was set in the same call.
    void
    reconcileUnits();
    // Marks the stored value as matching the owner units after a conversion.
    void
    adoptOwnerUnits();

private:
    bool
    ownerUsesPixels();
    GraphicsObject* m_owner = nullptr;
    bool m_shiftedForPixels = true;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
