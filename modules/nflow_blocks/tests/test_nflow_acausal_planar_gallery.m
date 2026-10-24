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
% A planar 2-D single rod pendulum opened from the acausal gallery and
% simulated. A uniform rod (length 1, mass 1, so I_com = 1/12 and I_pivot = 1/3)
% pivoted at one end and released horizontally obeys the physical-pendulum
% equation I_pivot phi'' = -m g d cos(phi), with d = 0.5 the pivot-to-centre
% distance. The check is an independent ode45 of that equation on the
% simulation grid, so a wrong revolute constraint, body inertia, gravity term or
% constraint stabilisation moves the answer away from it.
%=============================================================================
o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_Planar_Pendulum.nflow']);
t = o.tout(:);
phi = o.phi(:);
%=============================================================================
Ip = 1 / 3;
d = 0.5;
mg = 9.81;
[~, yr] = ode45(@(tt, y) [y(2); -(mg * d / Ip) * cos(y(1))], t, [0; 0], ...
  odeset('RelTol', 1e-9, 'AbsTol', 1e-11));
assert_istrue(max(abs(phi - yr(:, 1))) < 1e-3);
%=============================================================================
