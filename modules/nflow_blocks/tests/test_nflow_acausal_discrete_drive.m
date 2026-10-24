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
% A discrete / sampled signal driving an acausal (DAE) source. The sampled
% output jumps at each sample hit; the variable-step implicit solver must
% reinitialise across that staircase discontinuity instead of carrying a stale
% multistep history (which used to fail with "IDASolve failed with flag -3").
% Two cases: an open-loop staircase into a SignalVoltage must run to completion,
% and a stable sampled-data P-control loop around an acausal RC plant must
% converge to the analytic set point Kp/(Kp+1).
%=============================================================================
function c = ac(id, type, params)
  c = struct('id', id, 'type', type, 'x', 0, 'y', 0, 'rotation', 0, 'params', params);
end
%=============================================================================
function b = cb(id, type, ni, no, params)
  b = struct('id', id, 'type', type, 'inputs', ni, 'outputs', no, 'x', 0, 'y', 0, ...
    'rotation', 0, 'params', params);
end
%=============================================================================
function out = runmodel(blocks, wires, conns, runtime, dt)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'discrete_drive';
  d.runtime = runtime;
  d.sampleTime = dt;
  d.solver = 'dae';
  for k = 1:numel(blocks)
    blocks{k}.x = 60 + 80 * k;
    blocks{k}.y = 120;
  end
  d.blocks = blocks;
  d.connections = conns;
  pw = {};
  for k = 1:numel(wires)
    pw{end + 1} = struct('pins', {wires{k}}); %#ok<AGROW>
  end
  d.physicalWires = pw;
  m = [tempname(), '.nflow'];
  fid = fopen(m, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(m);
  rmfile(m);
end
%=============================================================================
% 1) Open-loop staircase: a ramp sampled-and-held (unit delay, Ts = 0.1) drives
%    a SignalVoltage into an RC. Must run to completion (no solver failure) and
%    stay finite.
o = runmodel({ ...
  cb('r', 'ramp', 0, 1, struct('slope', 1)), ...
  cb('ud', 'unitDelay', 1, 1, struct('InitialCondition', 0, 'SampleTime', 0.1)), ...
  ac('V1', 'SignalVoltage', struct()), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0.2, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), ...
  cb('tw', 'toWorkspace', 1, 0, struct('VariableName', 'vc', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'C1.p', 'VS.p'}, {'G.p', 'V1.n', 'C1.n', 'VS.n'}}, ...
  {struct('from', 'r', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ud', 'to', 'V1', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)}, 2, 0.02);
vc = o.vc(:);
assert_istrue(numel(vc) > 0 && all(isfinite(vc)));
% The held staircase lags the ramp by up to one sample; the RC output stays
% within the range the input spans.
assert_istrue(max(vc) < 2.2 && min(vc) >= -1e-6);
evalin('base', 'clear(''vc'')');
%=============================================================================
% 2) Stable sampled-data P-control of an acausal RC: u = Kp (setpoint - Vc),
%    sampled at Ts = 0.02. With Kp = 1 the closed loop settles at the analytic
%    Kp/(Kp+1) = 0.5. Locks the discrete-controller -> acausal-plant workflow.
Kp = 1;
o = runmodel({ ...
  ac('V1', 'SignalVoltage', struct()), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0.2, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), ...
  cb('sp', 'constant', 0, 1, struct('Value', 1)), ...
  cb('ud', 'unitDelay', 1, 1, struct('InitialCondition', 0, 'SampleTime', 0.02)), ...
  cb('sum', 'sum', 2, 1, struct('Inputs', '+-')), ...
  cb('K', 'gain', 1, 1, struct('Gain', Kp)), ...
  cb('tw', 'toWorkspace', 1, 0, struct('VariableName', 'vc', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'C1.p', 'VS.p'}, {'G.p', 'V1.n', 'C1.n', 'VS.n'}}, ...
  {struct('from', 'VS', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'sp', 'to', 'sum', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ud', 'to', 'sum', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'sum', 'to', 'K', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'K', 'to', 'V1', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)}, 4, 0.02);
vc = o.vc(:);
assert_istrue(abs(vc(end) - Kp / (Kp + 1)) < 1e-2);
evalin('base', 'clear(''vc'')');
%=============================================================================
