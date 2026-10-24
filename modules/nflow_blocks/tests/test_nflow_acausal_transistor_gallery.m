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
% Transistor operating points opened from the acausal gallery. The check is the
% device physics: the Ebers-Moll bipolar law and the square-law MOSFET, solved
% for the diode-connected bias point by bisection.
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
% Diode-connected NPN: (V - Vbe)/R = Is (1 + 1/Bf)(exp(Vbe/Vt) - 1).
o = G('NPN_Diode_Connected');
Vref = bisect(@(x)(5 - x) / 1000 - 1e-16 * (1 + 1 / 100) * (exp(x / 0.025) - 1), 0, 5);
assert_istrue(max(abs(o.vb(:) - Vref)) < 1e-6);
%=============================================================================
% Diode-connected PNP: Vn/R = Is (1 + 1/Bf)(exp((V - Vn)/Vt) - 1).
o = G('PNP_Diode_Connected');
Vref = bisect(@(x) x / 1000 - 1e-16 * (1 + 1 / 100) * (exp((5 - x) / 0.025) - 1), 0, 5);
assert_istrue(max(abs(o.vn(:) - Vref)) < 1e-6);
%=============================================================================
% Diode-connected PMOS: Vn/R = (Beta/2)(V - Vn - Vt)^2.
o = G('PMOS_Diode_Connected');
Vref = bisect(@(x) x / 1000 - (1e-3 / 2) * max(5 - x - 1, 0) ^ 2, 0, 5);
assert_istrue(max(abs(o.vn(:) - Vref)) < 1e-6);
%=============================================================================
