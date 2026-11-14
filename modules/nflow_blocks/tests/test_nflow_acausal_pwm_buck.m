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
% Buck converter whose gate is a comparator-based PWM modulator (a sawtooth
% carrier 0..1 compared to a constant duty by a relational operator), not a
% fixed pulse source. The comparator flip instant lands inside a carrier period,
% so the DAE solver must locate it as a zero-crossing of (carrier - duty) and
% schedule the switch there; a live gate read would flip the switch mid-step and
% the IDAS error test would fail. This exercises the event-aware switching path
% that lets a variable / closed-loop duty drive an ideal switch.
%=============================================================================
Vin = 12; D = 0.4; expected = D * Vin;  % trailing-edge PWM: Vout = D * Vin = 4.8 V
out = sim([modulepath('nflow_blocks'), '/examples/acausal/Acausal_Buck_Converter_PWM_Demo.nflow']);
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
