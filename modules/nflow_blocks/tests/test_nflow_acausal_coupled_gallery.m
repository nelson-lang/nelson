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
% Cross-domain machines opened from the acausal gallery: a DC motor (EMF,
% electrical <-> rotational) and a heating resistor (electrical -> thermal).
% Each is checked against an independent ode45 or closed form on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% DC motor: L di/dt = V - R i - k w, J dw/dt = k i - d w, all unity.
o = G('DC_Motor_EMF');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [(1 - y(1) - y(2)); (y(1) - y(2))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.imot(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.wmot(:) - yr(:, 2))) < 1e-4);
%=============================================================================
% Heating resistor: C dT/dt = V^2/R - G(T - Tamb), settles at 22 C.
o = G('Heating_Resistor');
t = o.tout(:);
assert_istrue(max(abs(o.Temp(:) - (20 + 2 * (1 - exp(-0.25 * t))))) < 1e-4);
%=============================================================================
