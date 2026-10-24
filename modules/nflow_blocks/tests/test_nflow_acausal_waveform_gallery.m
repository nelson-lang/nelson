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
% Waveform sources across the domains, opened from the acausal gallery: a sine
% force, a sine torque, an exponentially damped sine voltage, a ramp current and
% a sine current. Each drives a linear circuit or mechanism whose response is an
% independent ode45 on the grid (or a closed form for the ramp).
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Sine force on a mass-spring-damper: m x'' + d x' + k x = A sin(w t).
o = G('SineForce_MSD');
t = o.tout(:);
w = 2 * pi * 0.3;
[~, yr] = ode45(@(tt, y) [y(2); (2 * sin(w * tt) - 0.5 * y(2) - 4 * y(1))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.x(:) - yr(:, 1))) < 1e-4);
%=============================================================================
% Sine torque on an inertia-spring-damper.
o = G('SineTorque_ISD');
t = o.tout(:);
w = 2 * pi * 0.3;
[~, yr] = ode45(@(tt, y) [y(2); (2 * sin(w * tt) - 0.5 * y(2) - 4 * y(1))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.p(:) - yr(:, 1))) < 1e-4);
%=============================================================================
% Exponentially damped sine voltage into an RC.
o = G('ExpSine_RC');
t = o.tout(:);
w = 2 * pi * 1;
src = @(tt) 2 * exp(-0.5 * tt) .* sin(w * tt);
[~, yr] = ode45(@(tt, y)(src(tt) - y) / (1 * 0.3), t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.vc(:) - yr)) < 1e-4);
%=============================================================================
% Ramp current into a capacitor: Vc = slope t^2/(2 C) = 2 t^2.
o = G('RampCurrent_Cap');
t = o.tout(:);
assert_istrue(max(abs(o.vc(:) - 2 * t .^ 2)) < 1e-4);
%=============================================================================
% Sine current into a parallel RC: C Vc' = i - Vc/R.
o = G('SineCurrent_RC');
t = o.tout(:);
w = 2 * pi * 0.5;
[~, yr] = ode45(@(tt, y)(sin(w * tt) - y / 2) / 0.5, t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.v(:) - yr)) < 1e-4);
%=============================================================================
