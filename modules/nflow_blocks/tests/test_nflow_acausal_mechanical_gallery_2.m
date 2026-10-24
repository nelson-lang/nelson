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
% More translational mechanisms opened from the acausal gallery: a two-mass
% coupled oscillator, quadratic drag, a linear speed force, a prescribed
% constant speed and a lever ratio. Each is checked against a closed form or an
% independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Two-mass chain: m1 x1'' = -k1 x1 - k2(x1 - x2), m2 x2'' = -k2(x2 - x1).
o = G('TwoMass_Chain');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [y(3); y(4); (-2 * y(1) - (y(1) - y(2))); (-(y(2) - y(1)))], ...
  t, [1; 0; 0; 0], odeset('RelTol', 1e-9, 'AbsTol', 1e-11));
assert_istrue(max(abs(o.x1(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.x2(:) - yr(:, 2))) < 1e-4);
%=============================================================================
o = G('Quadratic_Drag');
t = o.tout(:);
assert_istrue(max(abs(o.v(:) - 4 ./ (1 + 2 * t))) < 1e-4);
%=============================================================================
o = G('Linear_SpeedForce');
t = o.tout(:);
assert_istrue(max(abs(o.v(:) - 3 * exp(-0.5 * t))) < 1e-4);
%=============================================================================
o = G('Constant_Speed');
t = o.tout(:);
assert_istrue(max(abs(o.s(:) - (1 + 2 * t))) < 1e-4);
%=============================================================================
o = G('Lever_Ratio');
t = o.tout(:);
assert_istrue(max(abs(o.sa(:) - 0.5 * 2 * t)) < 1e-4);
%=============================================================================
