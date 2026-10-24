%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
% bitand / bitor / bitxor on the device, every integer class, matching the host
classes = {'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32'};
for k = 1:numel(classes)
  cls = classes{k};
  a = cast([12 200 255 1], cls);
  b = cast([10 150 15 3], cls);
  A = gpuArray(a);
  B = gpuArray(b);
  assert_isequal(gather(bitand(A, B)), bitand(a, b));
  assert_isequal(gather(bitor(A, B)), bitor(a, b));
  assert_isequal(gather(bitxor(A, B)), bitxor(a, b));
  assert_isequal(underlyingType(bitand(A, B)), cls);
end
%=============================================================================
% negative signed values use the two's-complement pattern
assert_isequal(gather(bitand(gpuArray(int8(-1)), gpuArray(int8(3)))), bitand(int8(-1), int8(3)));
assert_isequal(gather(bitor(gpuArray(int16(-256)), gpuArray(int16(255)))), ...
  bitor(int16(-256), int16(255)));
%=============================================================================
% bitshift on int32/uint32 (left, right, width-truncating overflow)
assert_isequal(gather(bitshift(gpuArray(uint32(1)), 3)), bitshift(uint32(1), 3));
assert_isequal(gather(bitshift(gpuArray(int32(-8)), -1)), bitshift(int32(-8), -1));
assert_isequal(gather(bitshift(gpuArray(uint32(1073741824)), 2)), bitshift(uint32(1073741824), 2));
assert_isequal(underlyingType(bitshift(gpuArray(int32(1)), 1)), 'int32');
%=============================================================================
% non-integer / int8 bitshift raise a clear error
assert_checkerror('bitand(gpuArray(single(1)), gpuArray(single(1)))', ...
  _('Bitwise operations require an integer gpuArray.'));
assert_checkerror('bitshift(gpuArray(int8(1)), 2)', ...
  _('bitshift on int8/int16 gpuArray is not supported; use int32/uint32.'));
%=============================================================================
