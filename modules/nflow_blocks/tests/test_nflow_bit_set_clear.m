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
% bitSet / bitClear: set or clear one bit (at BitIndex) of the
% integer-valued input.
%=============================================================================
% Each row: {type, BitIndex, input value, expected output}.
cases = {...
  {'bitSet', 0, 8, 9}, ...      % 1000 set bit0 -> 1001 = 9
  {'bitSet', 1, 8, 10}, ...     % 1000 set bit1 -> 1010 = 10
  {'bitSet', 3, 8, 8}, ...      % bit3 already 1 -> unchanged
  {'bitClear', 3, 8, 0}, ...    % 1000 clear bit3 -> 0
  {'bitClear', 0, 8, 8}, ...    % bit0 already 0 -> unchanged
  {'bitClear', 1, 11, 9} ...    % 1011 clear bit1 -> 1001 = 9
};
for k = 1:numel(cases)
  c = cases{k};
  ty = c{1}; idx = c{2}; value = c{3}; expected = c{4};
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', value)), ...
    struct('id', 'bb', 'type', ty, 'inputs', 1, 'outputs', 1, ...
      'params', struct('BitIndex', idx, 'NumBits', 8)), ...
    struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'bc_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'bb', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'bb', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', 'bc_out'); y = y(:)';
  assert_isequal(y(1), expected);
end
%=============================================================================
