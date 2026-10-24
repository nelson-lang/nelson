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
% The sine-driven series RLC demo (examples/acausal/Acausal_Series_RLC_AC_Demo
% .nflow) opened and simulated. The circuit obeys, for the capacitor voltage Vc
% and the loop current i,
%   L C Vc'' + R C Vc' + Vc = A sin(2 pi f t),   i = C Vc'.
% The check is an independent ode45 of that governing equation sampled on the
% simulation grid, so an error in the SineVoltage source, in the island
% assembly, in the DAE residual or in the VoltageSensor / CurrentSensor moves
% the answer away from it. Parameters A = 1, f = 0.5, R = 1, L = 1, C = 0.5.
%=============================================================================
model = [modulepath('nflow_blocks'), '/examples/acausal/Acausal_Series_RLC_AC_Demo.nflow'];
out = sim(model);
t = out.tout(:);
Vc = out.vc(:);
Ic = out.il(:);
%=============================================================================
A = 1; f = 0.5; R = 1; L = 1; C = 0.5;
w = 2 * pi * f;
odef = @(tt, y) [y(2); (A * sin(w * tt) - R * C * y(2) - y(1)) / (L * C)];
[~, yr] = ode45(odef, t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
Vc_ref = yr(:, 1);
Ic_ref = C * yr(:, 2);
%=============================================================================
assert_istrue(max(abs(Vc - Vc_ref)) < 1e-4);
assert_istrue(max(abs(Ic - Ic_ref)) < 1e-4);
%=============================================================================
