//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <memory>
#include <mutex>
#include "Evaluator.hpp"
#include "EvaluateInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class ParallelEvaluator
{
public:
    static std::shared_ptr<Evaluator>
    create(EvaluateInterface* evaluatorInterface, size_t ID);

    static void
    destroy(std::shared_ptr<Evaluator>& evaluator, bool evaluatorWasCanceled);

private:
    ParallelEvaluator() = delete;
    ~ParallelEvaluator() = delete;
    ParallelEvaluator(const ParallelEvaluator&) = delete;
    ParallelEvaluator&
    operator=(const ParallelEvaluator&)
        = delete;

    static std::mutex evaluatorMutex;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
