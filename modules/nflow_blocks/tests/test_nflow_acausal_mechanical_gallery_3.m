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
% A mass under gravity, a relative-position sensor across two free masses, and a
% linear motor (TranslationalEMF), opened from the acausal gallery. Each is
% checked against a closed form or an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Free fall: s = s0 - (1/2) g t^2, s0 = 10, g = 9.81.
o = G('MassWithWeight_FreeFall');
t = o.tout(:);
assert_istrue(max(abs(o.x(:) - (10 - 0.5 * 9.81 * t .^ 2))) < 1e-4);
%=============================================================================
% Relative position of two free masses on a spring: x1 - x2, k = 3, v0 = [1,0].
o = G('RelPosition_TwoMass');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [y(3); y(4); -3 * (y(1) - y(2)); 3 * (y(1) - y(2))], ...
  t, [0; 0; 1; 0], odeset('RelTol', 1e-9, 'AbsTol', 1e-11));
assert_istrue(max(abs(o.dx(:) - (yr(:, 1) - yr(:, 2)))) < 1e-4);
%=============================================================================
% Linear motor: L di/dt = V - R i - k v, m dv/dt = k i - d v, all unity.
o = G('Linear_Motor_TEMF');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [(1 - y(1) - y(2)); (y(1) - y(2))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.imot(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.vmot(:) - yr(:, 2))) < 1e-4);
%=============================================================================
