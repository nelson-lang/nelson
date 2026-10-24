//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIComponent.h"
#include "GOPositionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIComponent::buildWidget(GOFigureHost* f)
{
    parentGoWindow = f;
}
//=============================================================================
void
GOUIComponent::syncPositionProperties()
{
    syncSimplePositionTriplet(this);
}
//=============================================================================
void
GOUIComponent::deleteWidget()
{
    if (binding) {
        binding->hide();
        delete binding;
        binding = nullptr;
    }
}
//=============================================================================
void
GOUIComponent::show()
{
    if (binding) {
        binding->show();
    }
}
//=============================================================================
void
GOUIComponent::hide()
{
    if (binding) {
        binding->hide();
    }
}
//=============================================================================
void
GOUIComponent::setFocus()
{
    if (binding) {
        binding->setFocus();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
