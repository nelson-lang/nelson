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
% Classic electrical circuits opened from the acausal gallery (examples/acausal/
% gallery/*.nflow) and simulated. Each circuit has an answer that can be written
% down (a closed form, an exact algebraic value, or an independent ode45 sampled
% on the simulation grid), so a wrong source, island assembly, DAE residual,
% sensor sign, current-source direction or op-amp constraint moves the answer
% away from the expected trajectory.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% RL series, step source: iL = (V/R)(1 - exp(-(R/L) t)) with V = 2, R = 4, L = 2.
o = G('RL_Step');
t = o.tout(:);
assert_istrue(max(abs(o.iL(:) - (2 / 4) * (1 - exp(-(4 / 2) * t)))) < 1e-4);
%=============================================================================
% Wheatstone bridge: V (R2/(R1+R2) - R4/(R3+R4)) = 20/21 with V = 10, R = 1..4.
o = G('Wheatstone_Bridge');
assert_istrue(max(abs(o.vg(:) - 10 * (2 / 3 - 4 / 7))) < 1e-9);
%=============================================================================
% Current divider: I R2/(R1+R2) = 2 A with I = 3, R1 = 2, R2 = 4.
o = G('Current_Divider');
assert_istrue(max(abs(o.ir1(:) - 2)) < 1e-9);
%=============================================================================
% Inverting op-amp: -(Rf/Rin) Vin = -3 V with Vin = 1, Rin = 1, Rf = 3.
o = G('Inverting_OpAmp');
assert_istrue(max(abs(o.vo(:) + 3)) < 1e-9);
%=============================================================================
% Op-amp integrator: -(1/(Rin C)) int Vin = -2 t with Vin = 1, Rin = 1, C = 0.5.
o = G('OpAmp_Integrator');
t = o.tout(:);
assert_istrue(max(abs(o.vo(:) - (-2 * t))) < 1e-4);
%=============================================================================
% RC low-pass, sine drive: C R Vc' + Vc = A sin(w t), A = 1, f = 1, R = 1, C = 0.2.
o = G('RC_LowPass_AC');
t = o.tout(:);
w = 2 * pi * 1;
[~, vref] = ode45(@(tt, y)(sin(w * tt) - y) / (1 * 0.2), t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.vc(:) - vref)) < 1e-4);
%=============================================================================
