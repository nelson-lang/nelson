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
% Extracting a range of bits yields them AT THE WEIGHT THEY HAD: bits 2..5 of
% 214 read 5 * 2^2 = 20, the number with everything outside the range masked
% away. Right-aligning them - 5 - is the other reading, and the one this block
% used to give with no way to ask for the first; it is still available under
% OutputScaling 'rightAligned'.
%=============================================================================
function y = eb_run(startBit, nbits, scaling, value)
  p = struct('StartBit', startBit, 'NumBitsToExtract', nbits, 'NumBits', 32);
  if ~isempty(scaling)
    p.OutputScaling = scaling;
  end
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'extract_bits_scaling';
  d.runtime = 0.2;
  d.sampleTime = 0.1;
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', value)), ...
    struct('id', 'e', 'type', 'extractBits', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', p), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_eb', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'e', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = out.y_eb(1);
  evalin('base', 'clear(''y_eb'')');
end
%=============================================================================
% 214 is 11010110. Bits 2..5 are 0101 = 5 right-aligned, 20 at their own weight
% (214 masked with 00111100). Bits 4..7 are 1101 = 13, or 208 in place.
assert_isequal(eb_run(2, 4, '', 214), 20);
assert_isequal(eb_run(2, 4, 'keepWeight', 214), 20);
assert_isequal(eb_run(2, 4, 'rightAligned', 214), 5);

assert_isequal(eb_run(4, 4, '', 214), 208);
assert_isequal(eb_run(4, 4, 'rightAligned', 214), 13);
%=============================================================================
% A range starting at bit 0 is the same number either way.
assert_isequal(eb_run(0, 4, '', 214), 6);
assert_isequal(eb_run(0, 4, 'rightAligned', 214), 6);
%=============================================================================
% Anything that is not one of the two spellings leaves the default in place
% rather than inventing a third behaviour.
assert_isequal(eb_run(2, 4, 'nonsense', 214), 20);
%=============================================================================
