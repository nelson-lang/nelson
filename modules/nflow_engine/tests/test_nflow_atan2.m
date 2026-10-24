%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A4.2: simulation-level coverage for the atan2 block (previously only exercised
% by code generation). out = atan2(y, x) with y on input port 0, x on port 1.
%=============================================================================
evalin('base', 'clear(''atan2_out'')');
asserts.isapprox(run_atan2(1, 1), pi / 4, 0, 1e-9); % y=x>0
asserts.isapprox(run_atan2(1, 0), pi / 2, 0, 1e-9); % on the +y axis: port order matters
asserts.isapprox(run_atan2(0, 1), 0, 0, 1e-9); % on the +x axis
asserts.isapprox(run_atan2(1, sqrt(3)), pi / 6, 0, 1e-9);
asserts.isapprox(run_atan2(-1, -1), -3 * pi / 4, 0, 1e-9); % third quadrant
evalin('base', 'clear(''atan2_out'')');
%=============================================================================
function y = run_atan2(yval, xval)
  d = struct();
  d.blocks = {...
    struct('id', 'y', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', yval)), ...
    struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', xval)), ...
    struct('id', 'a', 'type', 'atan2', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'atan2_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'y', 'to', 'a', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'x', 'to', 'a', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'a', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.5; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert(strcmp(r.status, 'done'));
  o = evalin('base', 'atan2_out');
  y = o(end);
end
%=============================================================================
