%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = tf([4.2,0.25,-0.004],[1,9.6,17]);
[z, gain] = zero(sys);
assert_isapprox(z, [-0.0726; 0.0131], 1e-3);
assert_isapprox(gain, 4.2000, 1e-4);
%=============================================================================
A = [-15, -20; 10, 0];
B = [5; 0];
C = [0, 10];
D = 0;
sys = ss(A, B, C, D);
[z, gain] = zero(sys);
assert_isequal(z, zeros(0, 1));
assert_isequal(real(gain), 500);
assert_istrue(imag(gain) < 100*eps);
%=============================================================================
