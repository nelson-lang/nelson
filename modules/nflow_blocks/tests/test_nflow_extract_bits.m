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
% extractBits: extracts NumBitsToExtract bits starting at
% StartBit (LSB, 0-based) from the integer-valued input, AT THEIR OWN WEIGHT -
% the bits are taken out of the number and the rest masked away. Right-aligning
% them is the other reading, under OutputScaling 'rightAligned'.
%=============================================================================
% Each row: {StartBit, NumBitsToExtract, input value, expected output}.
% 0xB4 = 180 = 10110100.
cases = {...
  {0, 4, 180, 4}, ...     % low nibble of 10110100 = 0100 = 4, weight 2^0
  {4, 4, 180, 176}, ...   % high nibble 1011 at weight 2^4 = 11 * 16
  {2, 3, 180, 20}, ...    % bits [4:2] = 101 at weight 2^2 = 5 * 4
  {0, 8, 180, 180}, ...   % whole byte
  {7, 1, 180, 128} ...    % top bit at weight 2^7
};
for k = 1:numel(cases)
  c = cases{k};
  sb = c{1}; nb = c{2}; value = c{3}; expected = c{4};
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', value)), ...
    struct('id', 'eb', 'type', 'extractBits', 'inputs', 1, 'outputs', 1, ...
      'params', struct('StartBit', sb, 'NumBitsToExtract', nb, 'NumBits', 8)), ...
    struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'eb_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'eb', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'eb', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', 'eb_out'); y = y(:)';
  assert_isequal(y(1), expected);
end
%=============================================================================
