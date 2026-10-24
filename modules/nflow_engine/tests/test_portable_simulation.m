%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
diagram = struct();
diagram.blocks = {...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 'sink', 'type', 'scope', 'inputs', 1, 'outputs', 0, ...
    'params', struct())};
diagram.connections = {struct('from', 'one', 'to', 'sink', ...
  'fromIndex', 0, 'toIndex', 0)};
diagram.sampleTime = 0.1;
diagram.duration = 0.2;
diagram.variables = struct();

result = jsondecode(__nflow_simulate__(jsonencode(diagram)));
asserts.isequal(result.status, 'done');
%=============================================================================
