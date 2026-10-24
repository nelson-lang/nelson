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
% Thermal networks opened from the acausal gallery: a two-stage heat chain, a
% body heated by a constant heat flow, and a heat-flow sensor on a cooling body.
% Each is checked against a closed form or an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Two-stage chain: C1 T1' = G1(Tamb-T1) - G2(T1-T2), C2 T2' = G2(T1-T2).
o = G('Thermal_Chain');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [(2 * (100 - y(1)) - (y(1) - y(2))); ((y(1) - y(2)) / 2)], ...
  t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.T1(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.T2(:) - yr(:, 2))) < 1e-4);
%=============================================================================
% Constant heat flow with conduction: T = Tamb + (Q/G)(1 - exp(-(G/C) t)).
o = G('HeatFlow_Body');
t = o.tout(:);
assert_istrue(max(abs(o.Temp(:) - (300 + 5 * (1 - exp(-0.5 * t))))) < 1e-4);
%=============================================================================
% Heat-flow sensor on a cooling body: Q = G(T - Tamb) = 80 exp(-0.5 t).
o = G('HeatFlowSensor_Cooling');
t = o.tout(:);
assert_istrue(max(abs(o.Q(:) - 80 * exp(-0.5 * t))) < 1e-4);
%=============================================================================
