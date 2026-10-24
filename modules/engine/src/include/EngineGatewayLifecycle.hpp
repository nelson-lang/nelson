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
namespace Nelson {
//=============================================================================
class Evaluator;
bool
registerEngineGateways(Evaluator* evaluator);
void
prepareEngineEvaluator(Evaluator* evaluator);
void
releaseEngineEvaluator(Evaluator* evaluator);
//=============================================================================
} // namespace Nelson
//=============================================================================
