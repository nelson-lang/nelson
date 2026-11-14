%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Open-loop buck converter from the acausal gallery. A 12 V DC source feeds an
% ideal high-side switch gated by a 10 kHz / 50% pulse, an ideal freewheeling
% diode and an LC output filter (L = 1 mH, C = 100 uF, R = 5). In continuous
% conduction the averaged output is Vout = D * Vin = 6 V, so this exercises the
% DAE solver on a hard-switching physical island (switch + freewheeling diode).
%=============================================================================
Vin = 12; D = 0.5; expected = D * Vin;  % = 6 V
out = sim([modulepath('nflow_blocks'), '/examples/acausal/Acausal_Buck_Converter_Demo.nflow']);
lg = out.logsout; nms = {lg.name};
v = lg(find(strcmp(nms, 'vout'), 1)).values;
v = v(:);
% Steady state: average the last third of the run.
n = numel(v);
tail = v(round(2 * n / 3):end);
vavg = mean(tail);
% Averaged step-down law Vout = D * Vin (small switch/diode drop tolerated).
assert_istrue(abs(vavg - expected) < 0.25);
% Physical sanity: bounded between 0 and the input rail, finite, and the
% steady-state ripple stays small (continuous-conduction LC filtering).
assert_istrue(~any(isnan(v)));
assert_istrue(min(v) > -0.05);
assert_istrue(max(v) < Vin + 2.5);
assert_istrue((max(tail) - min(tail)) < 0.2);
%=============================================================================
