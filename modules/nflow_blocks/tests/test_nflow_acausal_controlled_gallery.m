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
% The four controlled sources and two storage circuits opened from the acausal
% gallery. The controlled sources have exact algebraic answers; the storage
% circuits have a closed form or an independent ode45 on the grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
o = G('VCV_Amplifier');
assert_istrue(max(abs(o.vo(:) - 10)) < 1e-9);
%=============================================================================
o = G('VCC_Source');
assert_istrue(max(abs(o.vo(:) - 5)) < 1e-9);
%=============================================================================
o = G('CCV_Source');
assert_istrue(max(abs(o.vo(:) - 6)) < 1e-9);
%=============================================================================
o = G('CCC_Source');
assert_istrue(max(abs(o.vo(:) - 6)) < 1e-9);
%=============================================================================
% RC driven by a ramp: Vc = slope (t - RC(1 - exp(-t/RC))), slope = 2, RC = 0.5.
o = G('RC_Ramp');
t = o.tout(:);
rc = 0.5;
assert_istrue(max(abs(o.vc(:) - 2 * (t - rc * (1 - exp(-t / rc))))) < 1e-4);
%=============================================================================
% Two capacitors sharing charge through a resistor: common final voltage 2.5 V.
o = G('TwoCap_Sharing');
t = o.tout(:);
[~, yr] = ode45(@(tt, y) [-(y(1) - y(2)); (y(1) - y(2)) / 3], t, [10; 0], odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.v1(:) - yr(:, 1))) < 1e-4);
assert_istrue(max(abs(o.v2(:) - yr(:, 2))) < 1e-4);
assert_istrue(abs((1 * o.v1(end) + 3 * o.v2(end)) / 4 - 2.5) < 1e-3);
%=============================================================================
