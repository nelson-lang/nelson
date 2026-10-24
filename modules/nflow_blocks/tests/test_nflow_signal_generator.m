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
% signalGenerator: a configurable periodic source. Waveform is
% sine, square or sawtooth; Amplitude scales it; Frequency is in Hz. Sampled at
% 0.1 s over [0, 0.9] (10 samples).
%=============================================================================
tvec = (0:10) * 0.1;

% --- sine, A = 2, f = 1 Hz: 2*sin(2*pi*t) ---
evalin('base', 'clear(''sg_out'')');
d = struct();
d.blocks = {...
  struct('id', 'g', 'type', 'signalGenerator', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Waveform', 'sine', 'Amplitude', 2, 'Frequency', 1)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'sg_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'sg_out'); y = y(:)';
assert_isequal(numel(y), numel(tvec));
assert_istrue(max(abs(y - 2 * sin(2 * pi * tvec))) < 1e-6);
evalin('base', 'clear(''sg_out'')');

% --- square, A = 1, f = 1 Hz: the period opens LOW and turns at its half ---
evalin('base', 'clear(''sg_out'')');
d2 = d;
d2.blocks{1}.params.Waveform = 'square';
d2.blocks{1}.params.Amplitude = 1;
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'sg_out'); y2 = y2(:)';
% Read from the phase, not from sin(): sin(pi) is a positive residue in binary
% and would put the turn one sample late.
sq = 1 - 2 * double((tvec - floor(tvec)) < 0.5);
assert_istrue(max(abs(y2 - sq)) < 1e-6);
evalin('base', 'clear(''sg_out'')');

% --- sawtooth, A = 1, f = 1 Hz: ramps DOWN from +1 to -1 ---
evalin('base', 'clear(''sg_out'')');
d3 = d;
d3.blocks{1}.params.Waveform = 'sawtooth';
d3.blocks{1}.params.Amplitude = 1;
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
y3 = evalin('base', 'sg_out'); y3 = y3(:)';
saw = 1 - 2 * (tvec - floor(tvec));
assert_istrue(max(abs(y3 - saw)) < 1e-6);
evalin('base', 'clear(''sg_out'')');
%=============================================================================
