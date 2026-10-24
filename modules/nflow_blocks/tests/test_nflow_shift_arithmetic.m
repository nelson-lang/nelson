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
% shiftArithmetic: arithmetic bit shift of the integer-valued input.
% Left shifts multiply by 2^ShiftNumber; right shifts divide (sign-preserving).
% Row = {ShiftDirection, ShiftNumber, input value, expected output}. Written in
% a compact (low-whitespace) form on purpose.
%=============================================================================
cases = {{'Left', 3, 5, 40}, {'Right', 1, 40, 20}, {'Right', 2, 7, 1}, {'Right', 1, -8, -4}, {'Left', 0, 13, 13}};
for k = 1:numel(cases)
  c = cases{k}; dir = c{1}; num = c{2}; value = c{3}; expected = c{4};
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', value)), struct('id', 'sa', 'type', 'shiftArithmetic', 'inputs', 1, 'outputs', 1, 'params', struct('ShiftDirection', dir, 'ShiftNumber', num)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'sa_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'c', 'to', 'sa', 'fromIndex', 0, 'toIndex', 0), struct('from', 'sa', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', 'sa_out'); y = y(:)';
  assert_isequal(y(1), expected);
end
%=============================================================================
