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
% Nonlinear electrical devices opened from the acausal gallery. The check is
% the device physics: the Shockley diode and the square-law MOSFET solved for
% the operating point by bisection, and a rectifier ODE integrated by ode45 on
% the simulation grid.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
function x = bisect(f, lo, hi)
  for it = 1:300
    mid = (lo + hi) / 2;
    if sign(f(mid)) == sign(f(lo))
      lo = mid;
    else
      hi = mid;
    end
  end
  x = (lo + hi) / 2;
end
%=============================================================================
% Diode operating point: (V - Vd)/R = Is (exp(Vd/Vt) - 1), V = 5, R = 1000.
o = G('Diode_Operating_Point');
Vd = bisect(@(x)(5 - x) / 1000 - 1e-9 * (exp(x / 0.025) - 1), 0, 5);
assert_istrue(max(abs(o.vd(:) - Vd)) < 1e-6);
%=============================================================================
% Diode-connected NMOS: (V - Vn)/R = (Beta/2)(Vn - Vt)^2, Beta = 1e-3, Vt = 1.
o = G('NMOS_Diode_Connected');
Vn = bisect(@(x)(5 - x) / 1000 - (1e-3 / 2) * max(x - 1, 0) ^ 2, 0, 5);
assert_istrue(max(abs(o.vn(:) - Vn)) < 1e-6);
%=============================================================================
% Half-wave rectifier: C Vc' = Is (exp((Vs - Vc)/Vt) - 1) - Vc/Rload.
o = G('HalfWave_Rectifier');
t = o.tout(:);
w = 2 * pi * 1;
[~, vref] = ode45(@(tt, y)(1e-3 * (exp((5 * sin(w * tt) - y) / 0.5) - 1) - y / 100) / 0.05, ...
  t, 0, odeset('RelTol', 1e-8, 'AbsTol', 1e-10));
assert_istrue(max(abs(o.vc(:) - vref)) < 1e-3);
%=============================================================================
