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
// Host-runner main() emission for the generated C and Rust targets: the CSV
// reader / zero-order-hold input filler / output writer / arg parser that turn
// the pure model into a runnable `nflow_run` binary. Number literals arrive
// pre-formatted (the generators own ns()/rf()), so no numeric formatting here.
//=============================================================================
#include "nlsNflow_engine_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace nflow {
//=============================================================================
// One CSV column: the header/access label and its language type string
// (C: "int64_t"/"uint64_t"/..., Rust: "i64"/"u64"/...). An empty typeName means a
// plain f64/double lane.
struct CodegenIoColumn
{
    std::string label;
    std::string typeName;
};
//=============================================================================
struct CMainSpec
{
    std::string durationLiteral; // ns(diagram runtime)
    std::string dtLiteral; // ns(sample time)
    bool anyTypedInput = false;
    bool hasBusInputs = false;
    bool anyTypedOutput = false;
    bool hasBusOutputs = false;
    bool hasStopBlock = false;
    std::vector<CodegenIoColumn> inColumns;
    std::vector<CodegenIoColumn> outColumns;
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP std::vector<std::string>
emitCMainLines(const CMainSpec& s);
//=============================================================================
struct RustMainSpec
{
    std::string durationLiteral; // rf(diagram runtime)
    std::string dtLiteral; // rf(sample time)
    bool hasBusOutputs = false;
    bool hasStopBlock = false;
    std::vector<CodegenIoColumn> inColumns; // typeName = rtype
    std::vector<CodegenIoColumn> outColumns; // typeName unused (labels only)
    std::vector<std::string> outputNames; // raw output field names
};
//=============================================================================
NLSNFLOW_ENGINE_IMPEXP std::vector<std::string>
emitRustMainLines(const RustMainSpec& s);
//=============================================================================
} // namespace nflow
//=============================================================================
