%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% int64/uint64 with a double scalar: exact value, rounded half away from zero, saturated
%=============================================================================
i64 = @(hi, lo) int64(hi) * int64(1000000000) + int64(lo);
u64 = @(hi, lo) uint64(hi) * uint64(1000000000) + uint64(lo);
x = int64(6608872876801453);
big = int64(2)^53 + 1;
%=============================================================================
% times, both sides and all product forms
asserts.isequal(x * 1000, i64(6608872876, 801453000));
asserts.isequal(1000 * x, i64(6608872876, 801453000));
asserts.isequal(x .* 1000, i64(6608872876, 801453000));
asserts.isequal(1000 .* x, i64(6608872876, 801453000));
asserts.isequal(times(x, 1000), i64(6608872876, 801453000));
asserts.isequal(mtimes(1000, x), i64(6608872876, 801453000));
asserts.isequal(x * 2.5, i64(16522182, 192003633));
asserts.isequal(x * -2.5, -i64(16522182, 192003633));
asserts.isequal(x * 3, i64(19826618, 630404359));
asserts.isequal(x * 1024.75, i64(6772442480, 502288962));
asserts.isequal(big * 0.5, int64(4503599627370497));
asserts.isequal(-big * 0.5, int64(-4503599627370497));
asserts.isequal(big * 2.5, i64(22517998, 136852483));
asserts.isequal((int64(2)^62 + 1) * 0.5, i64(2305843009, 213693953));
asserts.isequal(i64(6608872876, 801453007) * (1 / 3), i64(2202957625, 600484213));
asserts.isequal(intmax('int64') * 0.5, int64(2)^62);
asserts.isequal(intmin('int64') * 0.5, -int64(2)^62);
asserts.isequal(intmin('int64') * -1.5, intmax('int64'));
asserts.isequal(x * 1e300, intmax('int64'));
asserts.isequal(x * -Inf, intmin('int64'));
asserts.isequal(int64(0) * Inf, int64(0));
asserts.isequal(x * NaN, int64(0));
asserts.isequal(x * 1e-300, int64(0));
%=============================================================================
asserts.isequal(uint64(6608872876801453) * 1000, u64(6608872876, 801453000));
asserts.isequal(intmax('uint64') * 0.5, uint64(2)^63);
asserts.isequal((uint64(2)^63 + 1) * 0.5, u64(4611686018, 427387905));
asserts.isequal((intmax('uint64') - 3) * 0.5, u64(9223372036, 854775806));
asserts.isequal((intmax('uint64') - 2) * 0.5, u64(9223372036, 854775807));
asserts.isequal(intmax('uint64') * 1.5, intmax('uint64'));
asserts.isequal(uint64(5) * -2.5, uint64(0));
%=============================================================================
% arrays with a scalar, on both sides
v = [x; big; int64(3); int64(-3); int64(0)];
asserts.isequal(v * 2.5, [i64(16522182, 192003633); i64(22517998, 136852483); 8; -8; 0]);
asserts.isequal(2.5 * v', [i64(16522182, 192003633), i64(22517998, 136852483), 8, -8, 0]);
asserts.isequal(class(v .* 2.5), 'int64');
asserts.isequal(size(int64(zeros(0, 3)) .* 2.5), [0 3]);
%=============================================================================
% right division x ./ d and x / d
asserts.isequal(int64(2)^62 / 1.5, i64(3074457345, 618258603));
asserts.isequal(intmax('int64') ./ 1.5, i64(6148914691, 236517205));
asserts.isequal(intmin('int64') ./ 1.5, -i64(6148914691, 236517205));
asserts.isequal(x ./ 0.1, i64(66088728, 768014526));
asserts.isequal(big / 0.5, i64(18014398, 509481986));
asserts.isequal(big / 1.5, int64(6004799503160662));
asserts.isequal(x / 0.49999999999999994, i64(13217745, 753602907));
asserts.isequal(intmax('int64') / 2^64, int64(0));
asserts.isequal(int64(2)^62 / 2^63, int64(1));
asserts.isequal(rdivide(int64(7), 0), intmax('int64'));
asserts.isequal(rdivide(int64(7), -0), intmin('int64'));
asserts.isequal(int64(-7) / 0, intmin('int64'));
asserts.isequal(int64(0) / 0, int64(0));
asserts.isequal(x / Inf, int64(0));
asserts.isequal(x / NaN, int64(0));
asserts.isequal(intmax('uint64') / 1.5, u64(12297829382, 473034410));
asserts.isequal(intmax('uint64') / 0.5, intmax('uint64'));
asserts.isequal(uint64(7) / -0, uint64(0));
asserts.isequal(uint64(7) / -2, uint64(0));
%=============================================================================
% double over integer d ./ x, x .\ d and d \ ... forms
asserts.isequal(1e19 ./ int64(3), i64(3333333333, 333333333));
asserts.isequal(int64(3) .\ 1e19, i64(3333333333, 333333333));
asserts.isequal(2^62 ./ int64(-1.5 * 2) .* -1, i64(1537228672, 809129301));
asserts.isequal(9.5e18 ./ uint64(7), u64(1357142857, 142857143));
asserts.isequal(9.5e18 ./ -big, int64(-1055));
asserts.isequal(2^64 ./ intmax('int64'), int64(2));
asserts.isequal(2^64 ./ intmax('uint64'), uint64(1));
asserts.isequal(3 ./ int64(0), intmax('int64'));
asserts.isequal(-3 ./ int64(0), intmin('int64'));
asserts.isequal(0 ./ int64(0), int64(0));
asserts.isequal(-Inf ./ intmin('int64'), intmax('int64'));
asserts.isequal(1.5 ./ intmax('int64'), int64(0));
asserts.isequal(mldivide(3, x), int64(2202957625600484));
asserts.isequal(mrdivide(v', 3), [int64(2202957625600484), int64(3002399751580331), 1, -1, 0]);
%=============================================================================
% a double from 2^64 up still gives an exact uint64 difference
asserts.isequal(2^64 - intmax('uint64'), uint64(1));
asserts.isequal(2^64 - (uint64(2)^63 + 1), u64(9223372036, 854775807));
asserts.isequal(2^64 - (intmax('uint64') - 3), uint64(4));
asserts.isequal(2^64 - uint64(0), intmax('uint64'));
asserts.isequal(2^65 - intmax('uint64'), intmax('uint64'));
asserts.isequal(uint64(5) - 2^64, uint64(0));
asserts.isequal(-2^64 + uint64(5), uint64(0));
%=============================================================================
% the fast double path and the exact path agree at the 2^53 limit
w = int64(2)^53 + int64(-3:3);
asserts.isequal(w * 0.5, int64(2)^52 + int64([-1 -1 0 0 1 1 2]));
asserts.isequal(w / 2, int64(2)^52 + int64([-1 -1 0 0 1 1 2]));
%=============================================================================
