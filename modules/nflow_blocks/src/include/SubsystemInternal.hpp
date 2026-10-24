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
#include "SimEngineTypes.hpp"
#include <memory>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Build the inner simulation state for a subsystem block. outerInWidths gives
    // the resolved width of each external-input port (from the outer signal layout),
    // so vector signals cross the boundary. Defined in SubsystemBuild.cpp; shared
    // with SubsystemHandlers.cpp (the handler + registration).
    //
    // layoutError, when given, receives the inner signal layout's diagnostic if
    // that layout is invalid (a width conflict, say). The caller must refuse the
    // diagram: building on a broken layout writes past the buffers.
    std::unique_ptr<BlockState::SubsystemState>
    buildSubsystemInnerState(const json& spec,
        const std::unordered_map<std::string, double>& outerVars, double dt,
        const std::vector<int>& outerInWidths, const std::vector<PortSig>* outerInSigs = nullptr,
        std::string* layoutError = nullptr);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
