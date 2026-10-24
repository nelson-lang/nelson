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
#include "ArrayOf.hpp"
#include "NFlowModelDocument.hpp"
#include <string>
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Resolve a model designator (numeric handle or char name; a "model/block"
// path resolves to its model) to a loaded document. Raises a Nelson error when
// the model is not loaded or the handle is invalid.
Nelson::NFlow::NFlowModelDocument&
requireModel(const ArrayOf& arg, std::string& modelName);
// Target of set_param/get_param: a model, or a specific block (when the
// designator is a "model/block" path).
struct ParamTarget
{
    Nelson::NFlow::NFlowModelDocument* doc = nullptr;
    std::string modelName;
    bool isBlock = false;
    std::string blockName;
};
//=============================================================================
ParamTarget
requireParamTarget(const ArrayOf& arg);
// Parse a "Block/port" reference (1-based port). Raises on malformed input.
void
parseBlockPort(const std::string& ref, std::string& block, int& port);
void
splitModelBlock(const std::string& path, std::string& model, std::string& block);
// ASCII lower-case copy (option-name matching).
std::string
toLowerCopy(const std::string& s);
// Convert a parameter value (char, numeric scalar or logical) to its string
// form, as stored in the model.
std::string
paramValueToString(const ArrayOf& v);
// True when v is a non-scalar, non-empty numeric array (vector or matrix): the
// case that must round-trip as a typed JSON value rather than a string.
bool
isNonScalarNumericValue(const ArrayOf& v);
// Convert a non-scalar numeric ArrayOf to a JSON value: a 1-D array for a row/
// column vector, a row-major array of arrays for a matrix.
nlohmann::json
numericArrayToJson(const ArrayOf& v);
// Convert a stored JSON parameter value back to an ArrayOf: number -> double
// scalar, 1-D number array -> double row vector, 2-D -> double matrix, bool ->
// logical, string -> char; anything else -> char array of its textual form.
ArrayOf
jsonParamToArrayOf(const nlohmann::json& v);
// Apply one block parameter with typed routing, shared by set_param and
// add_block: "Position" sets the geometry from [x y x2 y2]; a non-scalar
// numeric value is stored as a typed JSON vector/matrix; anything else uses the
// string convention. Raises a Nelson error on failure.
void
applyTypedBlockParam(Nelson::NFlow::NFlowModelDocument& doc, const std::string& blockName,
    const std::string& key, const ArrayOf& value);
void
raiseModelError(const std::string& message);
// Load the built-in block catalog (once) from the nflow_blocks libraries directory.
void
ensureCatalogLoaded();
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
