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
% Multi-node networks and a solver cross-check, opened from the acausal gallery.
% A two-stage RC ladder is checked against an independent ode45. A series RLC
% step is checked against its underdamped closed form AND re-simulated with the
% fixed-step ode4 solver to confirm the island gives consistent results across
% solvers. A wrong nodal assembly or a solver-specific defect moves the answer
% away from the expected trajectory.
%=============================================================================
function p = gpath(name)
  p = [modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow'];
end
%=============================================================================
% Two-stage RC ladder: C1 Vc1' = (Vin-Vc1)/R1 - (Vc1-Vc2)/R2, C2 Vc2' =
% (Vc1-Vc2)/R2, with Vin = 1, R1 = 1, C1 = 0.5, R2 = 2, C2 = 1. Expected: ode45.
o = sim(gpath('RC_Ladder'));
t = o.tout(:);
odef = @(tt, y) [((1 - y(1)) / 1 - (y(1) - y(2)) / 2) / 0.5; ((y(1) - y(2)) / 2) / 1];
[~, yr] = ode45(odef, t, [0; 0], odeset('RelTol', 1e-9, 'AbsTol', 1e-11));
assert_istrue(max(abs(o.vc2(:) - yr(:, 2))) < 1e-4);
%=============================================================================
% Series RLC step (R = L = C = 1), underdamped: Vc = 1 - exp(-alpha t)
% (cos(wd t) + (alpha/wd) sin(wd t)), alpha = 1/2, wd = sqrt(3)/2.
o = sim(gpath('Series_RLC_Step'));
t = o.tout(:);
vc_dae = o.vc(:);
alpha = 0.5;
wd = sqrt(3) / 2;
ref = 1 - exp(-alpha * t) .* (cos(wd * t) + (alpha / wd) * sin(wd * t));
assert_istrue(max(abs(vc_dae - ref)) < 1e-4);
%=============================================================================
% Same circuit under the fixed-step ode4 solver must agree with the DAE solve
% (the physical island self-integrates under an explicit solver).
o4 = sim(gpath('Series_RLC_Step'), 'Solver', 'ode4');
vc_ode4 = o4.vc(:);
n = min(numel(vc_dae), numel(vc_ode4));
assert_istrue(max(abs(vc_dae(1:n) - vc_ode4(1:n))) < 5e-3);
%=============================================================================
