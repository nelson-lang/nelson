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
% Translational and rotational mechanisms opened from the acausal gallery: an
% undamped mass-spring, a mass-damper decay, an inertia-spring, a rotational
% damper decay, a composite spring-damper step and a geared inertia. Each is
% checked against its closed form or an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
o = G('Mass_Spring');
t = o.tout(:);
assert_istrue(max(abs(o.x(:) - cos(2 * t))) < 1e-4);
%=============================================================================
o = G('Mass_Damper');
t = o.tout(:);
assert_istrue(max(abs(o.v(:) - 3 * exp(-0.5 * t))) < 1e-4);
%=============================================================================
o = G('Rot_Inertia_Spring');
t = o.tout(:);
assert_istrue(max(abs(o.p(:) - 0.5 * cos(2 * t))) < 1e-4);
%=============================================================================
o = G('Rot_Damper');
t = o.tout(:);
assert_istrue(max(abs(o.w(:) - 4 * exp(-0.5 * t))) < 1e-4);
%=============================================================================
% Composite spring-damper under a constant force: m x'' + d x' + k x = F.
o = G('SpringDamper_Step');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [y(2); (1 - 0.5 * y(2) - 4 * y(1))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.x(:) - yr(:, 1))) < 1e-4);
%=============================================================================
% Geared inertia: phi = tau/(2 Jeq) t^2, Jeq = J1 + J2/ratio^2 = 9.
o = G('Geared_Inertia');
t = o.tout(:);
assert_istrue(max(abs(o.p(:) - t .^ 2 / 9)) < 1e-4);
%=============================================================================
