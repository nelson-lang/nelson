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
% A pulse (square) source driving an acausal circuit, and a PWM-switched
% circuit. The pulse's hard edges are scheduled discontinuities, so the DAE
% solver stops just before each and restarts consistently instead of failing to
% integrate across the jump (it used to fail with "IDASolve failed flag -1").
% The PWM case also exercises an ideal switch toggling both ways at those edges.
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
  d.name = 'pwm';
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
% 1) A pulse voltage source (period 0.4, 50% duty) into an RC. It must run to
%    completion, stay within [0, source] and actually chop (min near 0, max
%    below the source since the RC never fully charges within a half period).
o = runmodel({ ...
  cb('p', 'pulse', 0, 1, struct('Amplitude', 1, 'Period', 0.4, 'Width', 50, 'StartTime', 0, 'Offset', 0)), ...
  ac('V1', 'SignalVoltage', struct()), ac('R1', 'Resistor', struct('R', 1)), ...
  ac('C1', 'Capacitor', struct('C', 0.2, 'v0', 0)), ac('VS', 'VoltageSensor', struct()), ...
  ac('G', 'Ground', struct()), ...
  cb('tw', 'toWorkspace', 1, 0, struct('VariableName', 'vc', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))}, ...
  {{'V1.p', 'R1.p'}, {'R1.n', 'C1.p', 'VS.p'}, {'G.p', 'V1.n', 'C1.n', 'VS.n'}}, ...
  {struct('from', 'p', 'to', 'V1', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)}, 2, 0.005);
vc = o.vc(:);
assert_istrue(numel(vc) > 0 && all(isfinite(vc)));
assert_istrue(min(vc) >= -1e-6 && max(vc) <= 1 + 1e-6);
assert_istrue(min(vc) < 0.2 && max(vc) > 0.5); % it genuinely chops
evalin('base', 'clear(''vc'')');
%=============================================================================
% 2) PWM-switched RC: the pulse drives an ideal switch that connects the source
%    to the RC only while high. The switch toggles both ways at the scheduled
%    edges; the run must complete and stay bounded.
o = sim_pwm();
vc = o.vc(:);
assert_istrue(numel(vc) > 0 && all(isfinite(vc)));
assert_istrue(min(vc) >= -1e-6 && max(vc) <= 1 + 1e-6);
evalin('base', 'clear(''vc'')');
%=============================================================================
function out = sim_pwm()
  isl = struct('id', 'isl', 'type', 'physicalIsland', 'inputs', 1, 'outputs', 1, ...
    'x', 200, 'y', 120, 'rotation', 0, 'params', struct('domain', 'electrical', 'nodes', 4, ...
    'components', {{struct('kind', 'vsource', 'a', 1, 'b', 0, 'V', 1), ...
      struct('kind', 'switch', 'a', 1, 'b', 3, 'port', 0), ...
      struct('kind', 'resistor', 'a', 3, 'b', 2, 'R', 1), ...
      struct('kind', 'capacitor', 'a', 2, 'b', 0, 'C', 0.2, 'ic', 0), ...
      struct('kind', 'voltageSensor', 'a', 2, 'b', 0, 'outPort', 0)}}));
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'pwmsw';
  d.runtime = 2;
  d.sampleTime = 0.005;
  d.solver = 'dae';
  d.blocks = { ...
    struct('id', 'p', 'type', 'pulse', 'inputs', 0, 'outputs', 1, 'x', 60, 'y', 120, ...
      'rotation', 0, 'params', struct('Amplitude', 1, 'Period', 0.4, 'Width', 50, ...
      'StartTime', 0, 'Offset', 0)), ...
    isl, ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'x', 340, 'y', 120, ...
      'rotation', 0, 'params', struct('VariableName', 'vc', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'p', 'to', 'isl', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'isl', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.physicalWires = {};
  m = [tempname(), '.nflow'];
  fid = fopen(m, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(m);
  rmfile(m);
end
%=============================================================================
