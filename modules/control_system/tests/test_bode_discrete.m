%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
H = tf(num, den, Ts);
bode(H)
%=============================================================================
[mag, phase, wout] = bode(H);
asserts.isequal(size(mag), [1 1 1000]);
asserts.isapprox(mag(1), -2.1829, 1e-4);
asserts.isapprox(mag(end), 2.0864, 1e-4);
asserts.isequal(size(phase), [1 1 1000]);
asserts.isapprox(phase(1), -0.0400, 1e-3);
asserts.isapprox(phase(end), -15.0717, 1e-4);
asserts.isequal(size(wout), [1 1000]);
asserts.isapprox(wout(1), 0.01, 1e-4);
asserts.isapprox(wout(end), 100, 1e-4);
%=============================================================================
