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
% Source and ideal-element checks opened from the acausal gallery: an
% exponentially damped sine force, a ramp force on a free mass, and the two
% ideal electrical limits Short (zero voltage) and Idle (zero current).
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Exponentially damped sine force on a mass-spring-damper.
o = G('ExpSineForce_MSD');
t = o.tout(:);
w = 2 * pi * 0.5;
src = @(tt) 3 * exp(-0.4 * tt) .* sin(w * tt);
[~, yr] = ode45(@(tt, y) [y(2); (src(tt) - 0.5 * y(2) - 4 * y(1))], t, [0; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.x(:) - yr(:, 1))) < 1e-4);
%=============================================================================
% Ramp force on a free mass: x = slope t^3/(6 m) = t^3/4.
o = G('RampForce_Mass');
t = o.tout(:);
assert_istrue(max(abs(o.x(:) - 3 * t .^ 3 / 12)) < 1e-4);
%=============================================================================
% Ideal short ties the node to ground.
o = G('Short_Circuit');
assert_istrue(max(abs(o.v(:))) < 1e-9);
%=============================================================================
% Ideal open branch carries no current, so the node sits at the source voltage.
o = G('Idle_Branch');
assert_istrue(max(abs(o.v(:) - 10)) < 1e-9);
%=============================================================================
