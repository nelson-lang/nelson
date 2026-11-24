//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The esbuild entry compiled to the standalone /uihtml-bootstrap.js asset (see
// vite.uihtml-bootstrap.ts). It runs the bridge in the real page; the logic
// lives in uihtmlBootstrap.ts so a unit test can import it without this side
// effect.
//=============================================================================
import { installUiHtmlBootstrap } from './uihtmlBootstrap';
//=============================================================================
installUiHtmlBootstrap();
//=============================================================================
