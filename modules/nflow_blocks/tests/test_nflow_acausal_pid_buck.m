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
% Closed-loop PID buck converter: a PID regulator drives the duty of a
% comparator-based PWM gate (sawtooth carrier < duty) that commands the ideal
% switch, closing the loop around the measured output voltage. This exercises
% the event-aware switching path with an algebraic (pid-produced) duty: the
% comparator crossing is located as a zero-crossing and the roots callback
% refreshes the regulator output before sampling the surface, so the loop
% regulates Vout to the reference independent of the input rail.
%=============================================================================
Vref = 5;
out = sim([modulepath('nflow_blocks'), '/examples/acausal/Acausal_Buck_Converter_PID_Demo.nflow']);
lg = out.logsout; nms = {lg.name};
v = lg(find(strcmp(nms, 'vout'), 1)).values;
v = v(:);
% Steady state: average the last third of the run.
n = numel(v);
tail = v(round(2 * n / 3):end);
vavg = mean(tail);
% The loop drives the output to the reference (small steady-state error).
assert_istrue(abs(vavg - Vref) < 0.15);
% Physical sanity: bounded, finite, no overshoot above the input rail, and the
% steady-state ripple stays small.
assert_istrue(~any(isnan(v)));
assert_istrue(min(v) > -0.05);
assert_istrue(max(v) < 12 + 1.0);
assert_istrue((max(tail) - min(tail)) < 0.2);
%=============================================================================
