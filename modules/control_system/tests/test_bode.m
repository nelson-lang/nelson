%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
H = tf([1 0.1 7.5],[1 0.12 9 0 0]);
[mag, phase, wout] = bode(H);
assert_isequal(size(mag), [1 1 1000]);
assert_isapprox(mag(1), 78.4163, 1e-4);
assert_isapprox(mag(end), -79.9987, 1e-4);
assert_isequal(size(phase), [1 1 1000]);
assert_isapprox(phase(1), -180, 1e-4);
assert_isapprox(phase(end), -179.9885, 1e-4);
assert_isequal(size(wout), [1 1000]);
assert_isapprox(wout(1), 0.01, 1e-4);
assert_isapprox(wout(end), 100, 1e-4);
%=============================================================================
H = tf([1 0.1 7.5],[1 0.12 9 0 0])
bode(H, '-.')
%=============================================================================
M = [1,2,3,4; 5 6 7 8; 8 9 10 11];
sys = tf(M);
assert_checkerror('[p, m , w] = bode(sys);', _('SISO LTI model expected.'))
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
H = tf(num, den, Ts);
bode(H)
%=============================================================================
[mag, phase, wout] = bode(H);
assert_isequal(size(mag), [1 1 1000]);
assert_isapprox(mag(1), -2.1829, 1e-4);
assert_isapprox(mag(end),  2.0864, 1e-4);
assert_isequal(size(phase), [1 1 1000]);
assert_isapprox(phase(1), -0.0400, 1e-3);
assert_isapprox(phase(end), -15.0717, 1e-4);
assert_isequal(size(wout), [1 1000]);
assert_isapprox(wout(1), 0.01, 1e-4);
assert_isapprox(wout(end), 100, 1e-4);
%=============================================================================
