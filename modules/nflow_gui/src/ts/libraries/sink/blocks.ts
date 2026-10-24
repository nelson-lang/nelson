//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { scope } from './scope.js';
import { xyScope } from './xyScope.js';
import { xyzScope } from './xyzScope.js';
import { labelSink } from './labelSink.js';
import { display } from './display.js';
//=============================================================================
export const blockDefinitions = {
  scope: { render: scope },
  xyScope: { render: xyScope },
  xyzScope: { render: xyzScope },
  labelSink: { render: labelSink },
  display: { render: display },
};
//=============================================================================
