%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% element-wise integer multiplication (.*) wrapped instead of saturating on
% the vector/matrix paths, and lost precision for 64-bit products because it
% used a floating-point accumulator instead of exact integer arithmetic
%=============================================================================
% scalar saturation
asserts.isequal(int8(100) .* int8(50), int8(127));
asserts.isequal(int8(-100) .* int8(50), int8(-128));
asserts.isequal(uint8(20) .* uint8(20), uint8(255));
%=============================================================================
% vector path saturation (regression: this wrapped to [-120 -120 120 -120])
asserts.isequal(int8([100 -100 40 -40]) .* int8([50 -50 3 3]), int8([127 127 120 -120]));
asserts.isequal(uint8([200 10 3]) .* uint8([2 30 4]), uint8([255 255 12]));
%=============================================================================
% matrix .* matrix saturation
A = int16([30000 -30000; 100 -100]);
B = int16([2 2; 3 3]);
asserts.isequal(A .* B, int16([32767 -32768; 300 -300]));
%=============================================================================
% scalar .* matrix and matrix .* scalar
asserts.isequal(int8(3) .* int8([50 -50 10]), int8([127 -128 30]));
asserts.isequal(int8([50 -50 10]) .* int8(3), int8([127 -128 30]));
%=============================================================================
% broadcasting: row .* column (outer product) saturates element-wise
r = int8([10 -10 40]);
c = int8([5; -20; 4]);
asserts.isequal(r .* c, int8([50 -50 127; -128 127 -128; 40 -40 127]));
%=============================================================================
% 64-bit products keep full precision (no double accumulator rounding).
% Reference values are built from pieces below 2^53 so the literals
% themselves stay exact (a decimal literal above 2^53 would be rounded by
% the double parser before the int64 cast).
ref1 = int64(1000000014) * int64(1000000000) + int64(49); % 1000000014000000049
ref2 = int64(9223372030) * int64(1000000000) + int64(926249001); % 9223372030926249001
ref3 = uint64(16) * uint64(1000000000) * uint64(1000000000); % 16000000000000000000
asserts.isequal(int64(1000000007) .* int64(1000000007), ref1);
asserts.isequal(int64(3037000499) .* int64(3037000499), ref2);
asserts.isequal(uint64(4000000000) .* uint64(4000000000), ref3);
%=============================================================================
% 64-bit saturation (regression: second element wrapped to intmin)
V = int64([1000000007 3037000500]) .* int64([1000000007 3037000500]);
asserts.isequal(V, [ref1 intmax('int64')]);
asserts.isequal(int64(-3037000500) .* int64(3037000500), intmin('int64'));
%=============================================================================
% result class is preserved
asserts.isequal(class(int32(2) .* int32(3)), 'int32');
asserts.isequal(class(uint64(2) .* uint64(3)), 'uint64');
%=============================================================================
