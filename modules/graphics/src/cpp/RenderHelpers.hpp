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
#include "RenderInterface.hpp"
//=============================================================================
void
DrawSymbol(RenderInterface& gc, RenderInterface::SymbolType symb, double x, double y, double z,
    double sze, std::vector<double> edgecolor, std::vector<double> fillcolor, double width);
//=============================================================================
RenderInterface::SymbolType
StringToSymbol(const std::wstring& symbolName);
//=============================================================================
