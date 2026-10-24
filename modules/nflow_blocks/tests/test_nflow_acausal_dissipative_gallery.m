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
% Two nonlinear dissipative elements opened from the acausal gallery:
% regularised Coulomb friction bringing a sliding mass to rest, and a body
% radiating heat to a fixed ambient (Stefan-Boltzmann). Each is checked against
% an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Coulomb friction: m v' = -Fc tanh(v/vEps), Fc = 2, v0 = 5.
o = G('Coulomb_Friction');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) -2 * tanh(y / 0.001), t, 5, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.v(:) - yr)) < 1e-3);
%=============================================================================
% Radiative cooling: C T' = -Gr (T^4 - Tamb^4), Gr = 0.1, T0 = 5, Tamb = 2.
o = G('Body_Radiation');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) -0.1 * (y ^ 4 - 2 ^ 4), t, 5, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.Temp(:) - yr)) < 1e-3);
%=============================================================================
