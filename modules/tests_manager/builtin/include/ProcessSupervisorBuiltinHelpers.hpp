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
#include <vector>
#include "ArrayOf.hpp"
#include "TestProcessSupervisor.hpp"
//=============================================================================
namespace Nelson::TestsManagerGateway {
//=============================================================================
std::vector<TestsManager::TestProcessJob>
processSupervisorJobsFromInput(const ArrayOfVector& argIn);
//=============================================================================
size_t
processSupervisorWorkerCount(const ArrayOfVector& argIn, size_t jobCount);
//=============================================================================
ArrayOf
processSupervisorEventsToStruct(const std::vector<TestsManager::TestProcessEvent>& events);
//=============================================================================
ArrayOf
processSupervisorResultsToStruct(const std::vector<TestsManager::TestProcessJob>& jobs,
    const std::vector<TestsManager::TestProcessResult>& results);
//=============================================================================
} // namespace Nelson::TestsManagerGateway
//=============================================================================
