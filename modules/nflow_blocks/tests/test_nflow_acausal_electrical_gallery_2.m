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
% More electrical circuits opened from the acausal gallery and simulated: a
% parallel RLC tank, an RL circuit under a sine drive, an RC high-pass, a
% gyrator terminated by a capacitor (which looks like an inductor) and a
% conductor divider. Each is checked against a closed form or an independent
% ode45 sampled on the simulation grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Parallel RLC: C V' = I - V/R - iL, L iL' = V, all unity.
o = G('Parallel_RLC');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [(1 - y(1) - y(2)); y(1)], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.v(:) - yr(:, 1))) < 1e-4);
%=============================================================================
% RL sine drive: L i' = A sin(w t) - R i, A = 2, f = 0.5, R = 1, L = 2.
o = G('RL_Sine');
t = o.tout(:);
w = 2 * pi * 0.5;
[~, iref] = ode45(@(tt, y)(2 * sin(w * tt) - y) / 2, t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.i(:) - iref)) < 1e-4);
%=============================================================================
% RC high-pass, output across the resistor: Vr = Vs - Vc, R C Vc' + Vc = Vs.
o = G('RC_HighPass');
t = o.tout(:);
w = 2 * pi * 1;
[~, vc] = ode45(@(tt, y)(sin(w * tt) - y) / (1 * 0.2), t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.vr(:) - (sin(w * t) - vc))) < 1e-4);
%=============================================================================
% Gyrator + capacitor as an inductor Leq = C/(G1 G2) = 2, driven through R = 1.
o = G('Gyrator_Inductor');
t = o.tout(:);
assert_istrue(max(abs(o.i(:) - (1 - exp(-0.5 * t)))) < 1e-4);
%=============================================================================
% Conductor divider: 12 * 4/(2+4) = 8 V.
o = G('Conductor_Divider');
assert_istrue(max(abs(o.v(:) - 8)) < 1e-9);
%=============================================================================
