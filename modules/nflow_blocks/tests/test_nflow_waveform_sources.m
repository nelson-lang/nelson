%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% Acausal waveform sources (SineVoltage / SineCurrent): NFlow.internal.acausalExpand
% splits each into a causal generator feeding a SignalVoltage / SignalCurrent,
% so a circuit driven by one simulates the corresponding waveform. No engine
% build needed (pure .m expansion). Only the smooth (sine) generators are
% exposed for now: a discontinuous source (pulse / step) makes the variable-step
% DAE solver fail at its edges.
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
function circuit = wav_circuit(srcType, srcParams)
  circuit = ['{"version":"1.0.0","format":"nflow-2","name":"Wav","blocks":[', ...
    '{"id":"V1","type":"', srcType, '","params":', srcParams, '},', ...
    '{"id":"R1","type":"Resistor","params":{"R":1}},', ...
    '{"id":"G","type":"Ground","params":{}},', ...
    '{"id":"VS","type":"VoltageSensor","params":{}},', ...
    '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
    '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
    '"physicalWires":[{"pins":["V1.p","R1.p","VS.p"]},', ...
    '{"pins":["R1.n","V1.n","VS.n","G.p"]}],', ...
    '"solver":"dae","runtime":1,"sampleTime":0.02}'];
end
%=============================================================================
% SineVoltage: v across the 1-ohm resistor is the source, +/- Amplitude.
ex = NFlow.internal.acausalExpand(wav_circuit('SineVoltage', '{"Amplitude":2,"Frequency":1,"Phase":0}'));
assert_istrue(~isempty(strfind(ex, '__wav'))); % causal generator added
assert_istrue(isempty(strfind(ex, 'SineVoltage'))); % rewritten to SignalVoltage
r = jsondecode(__nflow_simulate__(ex));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_istrue(max(v) > 1.9 && max(v) <= 2.01);
assert_istrue(min(v) < -1.9 && min(v) >= -2.01);
%=============================================================================
% SineCurrent: through a 1-ohm resistor, the sensed voltage equals the current.
ex = NFlow.internal.acausalExpand(wav_circuit('SineCurrent', '{"Amplitude":1.5,"Frequency":2,"Phase":0}'));
assert_istrue(~isempty(strfind(ex, '__wav')));
assert_istrue(isempty(strfind(ex, 'SineCurrent')));
r = jsondecode(__nflow_simulate__(ex));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_istrue(max(abs(v)) > 1.4 && max(abs(v)) <= 1.52);
%=============================================================================
% ExpSineVoltage: a decaying sine (continuous), peaks near +Amplitude early.
ex = NFlow.internal.acausalExpand(wav_circuit('ExpSineVoltage', '{"Amplitude":2,"Frequency":2,"Damping":0.5,"Phase":0}'));
r = jsondecode(__nflow_simulate__(ex));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_istrue(max(v) > 1.0 && max(v) <= 2.01);
%=============================================================================
% TrapezoidVoltage: continuous ramp-up / hold / ramp-down, holds at Amplitude.
ex = NFlow.internal.acausalExpand(wav_circuit('TrapezoidVoltage', '{"Amplitude":2,"Rising":0.2,"Width":0.3,"Falling":0.2,"Period":1}'));
r = jsondecode(__nflow_simulate__(ex));
assert_isequal(r.status, 'done');
v = r.scopes(1).series(:);
assert_istrue(max(v) > 1.9 && max(v) <= 2.01);
assert_istrue(min(v) >= -0.01);
%=============================================================================
