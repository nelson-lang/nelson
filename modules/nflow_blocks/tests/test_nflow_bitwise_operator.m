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
% bitwiseOperator: bit-wise logic between the integer-valued
% input and a constant BitMask. Values are reinterpreted as NumBits-wide
% unsigned integers.
%=============================================================================
% Each row: {Operation, input value, BitMask, NumBits, expected output}.
% 12 = 1100, 10 = 1010.
cases = {...
  {'AND', 12, 10, 8, 8}, ...    % 1100 & 1010 = 1000
  {'OR', 12, 10, 8, 14}, ...    % 1100 | 1010 = 1110
  {'XOR', 12, 10, 8, 6}, ...    % 1100 ^ 1010 = 0110
  {'NOT', 12, 0, 8, 243}, ...   % ~1100 over 8 bits = 11110011
  {'NAND', 12, 10, 8, 247}, ... % ~(1000) over 8 bits = 11110111
  {'NOR', 12, 10, 8, 241}, ...  % ~(1110) over 8 bits = 11110001
  {'AND', 255, 255, 4, 15} ...  % both masked to 4 bits (1111) -> 1111
};
for k = 1:numel(cases)
  c = cases{k};
  op = c{1}; value = c{2}; mask = c{3}; numbits = c{4}; expected = c{5};
  evalin('base', 'clear(''bw_out'')');
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', value)), ...
    struct('id', 'bw', 'type', 'bitwiseOperator', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Operation', op, 'BitMask', mask, 'NumBits', numbits)), ...
    struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'bw_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'bw', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'bw', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', 'bw_out'); y = y(:)';
  assert_isequal(y(1), expected);
end
evalin('base', 'clear(''bw_out'')');
%=============================================================================
