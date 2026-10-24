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
% A continuous block whose output carries a feedthrough term (a pid's kp*u)
% must read its input from the algebraic blocks feeding it, even under the
% native solver loop (ode1/ode4/ode45). Feedthrough routing blocks (gain, sum,
% ...) run only in the ALGEBRAIC pass, after the OUTPUT pass; the native loop
% now re-runs OUTPUT once the algebraic blocks are solved, so the pid term no
% longer reads a stale (zero) input. Before the fix the pid output collapsed to
% 0 under every explicit solver whenever a feedthrough block sat upstream, which
% also broke every pid -> plant control loop (the plant integrated a zero drive)
% while the default fixed-step path stayed correct.
%=============================================================================
function v = sim_var(d, varName)
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(r.status, 'done');
  v = evalin('base', varName);
  v = v(:);
  evalin('base', ['clear(''', varName, ''')']);
end
%=============================================================================
% Model 1: step -> gain -> pid (pure P) -> toWorkspace. The pid output is a pure
% feedthrough kp*u = 2*1 = 2, held for the whole run. Direct source -> pid always
% worked; the regression appeared only with a feedthrough block in between.
d1 = struct();
d1.blocks = {...
  struct('id', 'r', 'type', 'step', 'inputs', 0, 'outputs', 1, 'params', struct('Time', 0)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 1)), ...
  struct('id', 'c', 'type', 'pid', 'inputs', 1, 'outputs', 1, 'params', struct('P', 2, 'I', 0, 'D', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfsfc_u', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d1.connections = {...
  struct('from', 'r', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d1.sampleTime = 0.01;
d1.duration = 0.5;
d1.variables = struct();

solvers = {'', 'ode1', 'ode4', 'ode45'};
for k = 1:numel(solvers)
  d = d1;
  if ~isempty(solvers{k})
    d.solver = solvers{k};
  end
  u = sim_var(d, 'nfsfc_u');
  assert_istrue(numel(u) >= 2);
  % The controller output must be the feedthrough value 2, not the pre-fix 0.
  % Sample 1 is skipped: the default fixed-step path holds the feedthrough one
  % step (its t = 0 sample reads the not-yet-solved 0), an unrelated pre-existing
  % artifact; from sample 2 on every solver settles on the constant 2.
  assert_istrue(max(abs(u(2:end) - 2)) <= 1e-6);
end
%=============================================================================
% Model 2: unity-feedback loop step -> sum(+-) -> pid (pure P) -> tf plant, with
% the plant output fed back and logged. The explicit-solver trajectory must be a
% live control response (settling near the setpoint), matching the default
% fixed-step path, not the pre-fix frozen-at-zero plant.
d2 = struct();
d2.blocks = {...
  struct('id', 'r', 'type', 'step', 'inputs', 0, 'outputs', 1, 'params', struct('Time', 0)), ...
  struct('id', 'e', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '+-')), ...
  struct('id', 'c', 'type', 'pid', 'inputs', 1, 'outputs', 1, 'params', struct('P', 2, 'I', 0, 'D', 0)), ...
  struct('id', 'p', 'type', 'tf', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Numerator', [1], 'Denominator', [1, 0.5, 1])), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfsfc_p', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {...
  struct('from', 'r', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'p', 'to', 'e', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'e', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'p', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'p', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.01;
d2.duration = 8.0;
d2.variables = struct();

ref = sim_var(d2, 'nfsfc_p'); % default fixed-step reference
assert_istrue(max(abs(ref)) > 0.5); % the loop settles near the 1.0 setpoint

for solver = {'ode1', 'ode4', 'ode45'}
  d = d2;
  d.solver = solver{1};
  gen = sim_var(d, 'nfsfc_p');
  n = min(numel(ref), numel(gen));
  assert_istrue(n >= 2);
  % Live response, not the pre-fix frozen-at-zero plant.
  assert_istrue(max(abs(gen(1:n))) > 0.5);
  % Same trajectory as the default path (the difference is only the integration
  % method: native fixed / adaptive RK vs the default fused fixed step).
  assert_istrue(max(abs(gen(1:n) - ref(1:n))) <= 0.05);
end
%=============================================================================
% Model 3: a zero-crossing trigger surface reads a feedthrough continuous output.
% The trigger control is sine -> gain -> pid (pure P), so the surface samples the
% pid feedthrough kp*u = sine, whose rising zero is at t* = 0.735 (dt = 0.1, the
% edge falls between two samples). The body passes a ramp (slope 1), so the held
% output is the ramp AT THE FIRE INSTANT. The roots callback re-runs OUTPUT after
% the algebraic solve, so the surface reads the resolved pid output and localizes
% the crossing; before the fix it read the stale (zero) pid output, so the surface
% never crossed and the trigger never fired (output stuck at 0).
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, 'params', struct('label', 'in1')), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('label', 'out1'))};
inner.connections = {struct('from', 'in1', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
inner.controlPort = struct('kind', 'trigger', 'triggerType', 'rising');

d3 = struct('blocks', {{...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 1)), ...
  struct('id', 'ctl', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 1, 'Frequency', 0.5, 'Phase', -0.735 * pi)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 1)), ...
  struct('id', 'pc', 'type', 'pid', 'inputs', 1, 'outputs', 1, 'params', struct('P', 1, 'I', 0, 'D', 0)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', inner, 'controlPort', inner.controlPort)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfsfc_zc', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))}}, ...
  'connections', {{...
    struct('from', 'r', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ctl', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'pc', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'pc', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)}}, ...
  'sampleTime', 0.1, 'duration', 1.0, 'solver', 'ode4', 'variables', struct());

zc = sim_var(d3, 'nfsfc_zc');
assert_isequal(numel(zc), 11);
assert_istrue(max(abs(zc(1:8))) < 1e-12); % held 0 before the crossing
assert_istrue(max(abs(zc(9:11) - 0.735)) < 1e-6); % fired AT the localized edge
%=============================================================================
