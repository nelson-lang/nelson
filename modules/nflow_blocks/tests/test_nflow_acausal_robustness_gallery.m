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
% Adversarial regimes opened from the acausal gallery that stress the DAE island
% solver: a conservative LC tank and a conservative mass-spring over many periods
% (numerical damping would shrink the amplitude), a stiff fast RC, a very lightly
% damped high-Q RLC, an extreme gear ratio, and a deeply forward biased diode.
%=============================================================================
function o = G(name)
  o = sim([modulepath('nflow_blocks'), '/examples/acausal/gallery/Acausal_', name, '.nflow']);
end
%=============================================================================
function x = bisect(f, lo, hi)
  for it = 1:400
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
% Conservative LC over 50 s: Vc = cos(t) with no numerical damping.
o = G('LC_Conservative');
t = o.tout(:);
assert_istrue(max(abs(o.v(:) - cos(t))) < 1e-3);
%=============================================================================
% Conservative mass-spring over 50 s: x = cos(t).
o = G('MassSpring_LongRun');
t = o.tout(:);
assert_istrue(max(abs(o.x(:) - cos(t))) < 1e-3);
%=============================================================================
% Stiff fast RC: 1 - exp(-t/(RC)), RC = 10 ms.
o = G('Stiff_RC');
t = o.tout(:);
assert_istrue(max(abs(o.v(:) - (1 - exp(-t / 0.01)))) < 1e-4);
%=============================================================================
% High-Q RLC over 40 s: underdamped step response, R = 0.05, L = C = 1.
o = G('HighQ_RLC');
t = o.tout(:);
al = 0.025;
wd = sqrt(1 - al ^ 2);
ref = 1 - exp(-al * t) .* (cos(wd * t) + (al / wd) * sin(wd * t));
assert_istrue(max(abs(o.v(:) - ref)) < 1e-3);
%=============================================================================
% Extreme gear ratio 0.01: phi = t^2/(2 Jeq), Jeq = 10001.
o = G('Extreme_Gear');
t = o.tout(:);
Jeq = 1 + 1 / 0.01 ^ 2;
assert_istrue(max(abs(o.p(:) - t .^ 2 / (2 * Jeq))) < 1e-9);
%=============================================================================
% Deeply forward biased diode: (V - Vd)/R = Is (exp(Vd/Vt) - 1), V = 50, R = 10.
o = G('Deep_Diode');
Vd = bisect(@(x)(50 - x) / 10 - 1e-12 * (exp(x / 0.025) - 1), 0, 50);
assert_istrue(max(abs(o.vd(:) - Vd)) < 1e-6);
%=============================================================================
% Zero-ohm resistor (ideal jumper): must act as a short, not divide by zero.
% The mid node is pinned to 0 V and the loop carries V/R1 = 5/10 = 0.5 A.
o = G('Zero_Resistor_Short');
assert_istrue(max(abs(o.vmid(:))) < 1e-6);
assert_istrue(max(abs(o.iloop(:) - 0.5)) < 1e-6);
%=============================================================================
