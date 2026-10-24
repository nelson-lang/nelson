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
% Rotational mechanisms opened from the acausal gallery: a two-inertia torsional
% oscillator, quadratic rotational drag, bearing friction, a prescribed constant
% angular speed and a rotational spring-damper. Each is checked against a closed
% form or an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Torsional oscillator: J1 phi1'' = -c1 phi1 - c2(phi1 - phi2), etc.
o = G('Torsional_Oscillator');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [y(3); y(4); (-2 * y(1) - (y(1) - y(2))); (-(y(2) - y(1)))], ...
  t, [1; 0; 0; 0], odeset('RelTol', 1e-9, 'AbsTol', 1e-11));
assert_istrue(max(abs(o.p1(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.p2(:) - yr(:, 2))) < 1e-4);
%=============================================================================
o = G('Rot_Quadratic_Drag');
t = o.tout(:);
assert_istrue(max(abs(o.w(:) - 4 ./ (1 + 2 * t))) < 1e-4);
%=============================================================================
% Bearing friction: J w' = -tau_c tanh(w/w_eps).
o = G('Bearing_Friction');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) -1 * tanh(y / 0.001), t, 4, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.w(:) - yr)) < 1e-3);
%=============================================================================
o = G('Constant_RotSpeed');
t = o.tout(:);
assert_istrue(max(abs(o.p(:) - (1 + 2 * t))) < 1e-4);
%=============================================================================
% Rotational spring-damper: J phi'' + d phi' + c phi = tau.
o = G('RotSpringDamper');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [y(2); (1 - 0.5 * y(2) - 4 * y(1))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.p(:) - yr(:, 1))) < 1e-4);
%=============================================================================
