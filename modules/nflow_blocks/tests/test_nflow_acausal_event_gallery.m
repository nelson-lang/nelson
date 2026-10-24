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
% Event-like rotational elements opened from the acausal gallery: a clutch
% bringing two inertias to a common speed, a freewheel that couples in one
% direction only, and a signal-actuated brake. The checks are physical
% invariants (angular momentum conservation, one-way coupling) and an
% independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
% Clutch: two unit inertias (w0 = 4 and 0) reach a common speed of 2, and the
% total angular momentum stays equal to 4 throughout.
o = G('Clutch_TwoInertia');
w1 = o.w1(:);
w2 = o.w2(:);
assert_istrue(max(abs(1 * w1 + 1 * w2 - 4)) < 1e-6);
assert_istrue(abs(w1(end) - 2) < 1e-2 && abs(w2(end) - 2) < 1e-2);
%=============================================================================
% Freewheel engaged (a overruns b): the freewheel couples and brakes to rest.
o = G('Freewheel_Engaged');
w1 = o.w1(:);
assert_istrue(abs(w1(end)) < 1e-2);
%=============================================================================
% Freewheel free (a does not overrun b): the inertia keeps its speed of 4.
o = G('Freewheel_Free');
w1 = o.w1(:);
assert_istrue(abs(w1(end) - 4) < 1e-2);
%=============================================================================
% Signal-actuated brake: J w' = -f tanh(w/w_eps), f = 2, w0 = 4.
o = G('Brake_Signal');
w1 = o.w1(:);
t = o.tout(:);
[~, yr] = ode45(@(tt, y) -2 * tanh(y / 0.001), t, 4, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(w1 - yr)) < 1e-3);
%=============================================================================
