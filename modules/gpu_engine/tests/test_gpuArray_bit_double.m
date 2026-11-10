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
% floating-point gpuArray values are handled as double by the bit functions
R = bitget(gpuArray([5 6]), 1);
assert_isequal(class(R), 'gpuArray');
assert_isequal(double(gather(R)), [1 0]);
assert_isequal(double(gather(bitget(gpuArray(5), 1:3))), [1 0 1]);
assert_isequal(double(gather(bitset(gpuArray(5), 2))), 7);
assert_isequal(double(gather(bitset(gpuArray(5), 2, 0))), 5);
R = bitcmp(gpuArray(5));
assert_isequal(class(R), 'gpuArray');
assert_istrue(abs(double(gather(R)) - bitcmp(5)) <= eps(single(bitcmp(5))));
msg = _('Double inputs must be nonnegative whole numbers less than 2^64.');
assert_checkerror('bitget(gpuArray(5.5), 1)', msg, 'Nelson:operators:bitDoubleOutOfRange');
%=============================================================================
% integer gpuArray values keep their class
assert_isequal(gather(bitget(gpuArray(uint8([5 6])), 1)), uint8([1 0]));
assert_isequal(gather(bitcmp(gpuArray(uint8(5)))), uint8(250));
assert_isequal(gather(bitget(gpuArray(int8(-1)), 8)), int8(1));
%=============================================================================
