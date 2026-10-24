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
% Continuous 3rd-order system 1/(s(s+1)(s+2)): finite gain and phase margins.
H = tf(1, [1 3 2 0]);
[Gm, Pm, Wcg, Wcp] = margin(H);
asserts.isapprox(Gm, 6, 1e-2);
asserts.isapprox(Pm, 53.41089242, 1e-2);
asserts.isapprox(Wcg, 1.414213562, 1e-2);
asserts.isapprox(Wcp, 0.4457465448, 1e-2);
%=============================================================================
% Infinite gain margin 10/((s+1)(s+2)): relative degree 2, phase reaches
% -180 deg only at infinity, so Gm and Wcg are both Inf.
H = tf(10, [1 3 2]);
[Gm, Pm, Wcg, Wcp] = margin(H);
asserts.istrue(isinf(Gm));
asserts.istrue(isinf(Wcg));
asserts.isapprox(Pm, 55.8621067, 1e-2);
asserts.isapprox(Wcp, 2.758958751, 1e-2);
%=============================================================================
% Relative degree 1 (1/(s+1)): the phase never reaches -180 deg, so the gain
% margin is infinite and the phase crossover frequency is NaN.
H = tf(1, [1 1]);
[Gm, ~, Wcg] = margin(H);
asserts.istrue(isinf(Gm));
asserts.istrue(isnan(Wcg));
%=============================================================================
% Unstable closed loop 100/(s(s+1)(s+2)): gain margin below 1, negative phase margin.
H = tf(100, [1 3 2 0]);
[Gm, Pm, Wcg, Wcp] = margin(H);
asserts.isapprox(Gm, 0.06, 1e-2);
asserts.isapprox(Pm, -53.252755, 1e-2);
asserts.isapprox(Wcg, 1.414213562, 1e-2);
asserts.isapprox(Wcp, 4.4657439, 1e-2);
%=============================================================================
% Discrete system (zero-order hold, Ts = 0.1 s).
H = c2d(tf(1, [1 3 2 0]), 0.1);
[Gm, Pm, Wcg, Wcp] = margin(H);
asserts.isapprox(Gm, 5.232613551, 1e-2);
asserts.isapprox(Pm, 52.1361597, 1e-2);
asserts.isapprox(Wcg, 1.318774097, 1e-2);
asserts.isapprox(Wcp, 0.4457178213, 1e-2);
%=============================================================================
% Frequency-response data form margin(mag, phase, w).
H = tf(1, [1 3 2 0]);
w = logspace(-2, 2, 2000);
[mag_db, phase_deg] = bode(H, w);
mag = 10 .^ (squeeze(mag_db) / 20);
phase_deg = squeeze(phase_deg);
[Gm, Pm, Wcg, Wcp] = margin(mag, phase_deg, w);
asserts.isapprox(Gm, 6, 1e-2);
asserts.isapprox(Pm, 53.41089242, 1e-2);
asserts.isapprox(Wcg, 1.414213562, 1e-2);
asserts.isapprox(Wcp, 0.4457465448, 1e-2);
%=============================================================================
% Only the first output: gain margin alone.
Gm = margin(tf(1, [1 3 2 0]));
asserts.isapprox(Gm, 6, 1e-2);
%=============================================================================
% margin(sys) with no output argument plots without error.
margin(tf(1, [1 3 2 0]));
%=============================================================================
% A MIMO model is rejected.
M = [1, 2, 3, 4; 5 6 7 8; 8 9 10 11];
sys = tf(M);
asserts.checkerror('[Gm, Pm, Wcg, Wcp] = margin(sys);', sprintf(_('%s expected.'), 'SISO LTI model'));
%=============================================================================
% The data form requires output arguments.
asserts.checkerror('margin([1 2], [0 -90], [1 2]);', _('margin with frequency-response data requires output arguments.'));
%=============================================================================
