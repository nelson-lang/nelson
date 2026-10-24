%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% A signal-driven physicalIsland must be correct under EVERY loop, not only
% under the IDAS ('dae') backend:
%   - a linear SISO island is reduced to a causal stateSpace before the run
%     (the same reduction pass code generation uses), so the explicit solvers
%     integrate it at full order;
%   - a non-reducible island (constant sources, nonlinear components) advances
%     itself with an embedded implicit (backward-Euler + Newton) step under
%     the fixed-step loop and the native ode1/ode4/ode45 steppers, mirroring
%     the generated code;
%   - under the stateful CVODES-session backends a remaining island is a typed
%     rejection pointing at 'dae' (their internal multistep history would
%     silently ignore the per-step island writes).
% Historical failure pinned here: the default loop returned an identically
% zero trajectory and ode4 diverged (~1e3 after 1 s) on the RC + sine model.
%=============================================================================
function y = isl_run(dj, overrides, varName)
  r = jsondecode(__nflow_simulate__(dj, overrides));
  assert_istrue(strcmp(r.status, 'done'));
  y = evalin('base', varName);
  y = y(:);
end
%=============================================================================
function dj = isl_rc_sine_diagram()
  % RC divider driven by a sine source, sensed across the capacitor
  % (R = 100, C = 1e-3 -> tau = 0.1 s). Linear SISO island: reduced.
  d = struct();
  d.blocks = {...
    struct('id', 'V1', 'type', 'SineVoltage', 'params', struct('Amplitude', 1, 'Frequency', 1, 'Phase', 0)), ...
    struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 100)), ...
    struct('id', 'C1', 'type', 'Capacitor', 'params', struct('C', 0.001, 'v0', 0)), ...
    struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
    struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'nfism_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.physicalWires = {...
    struct('pins', {{'V1.p', 'R1.p'}}), ...
    struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), ...
    struct('pins', {{'V1.n', 'C1.n', 'VS.n', 'G.p'}})};
  d.sampleTime = 0.01;
  d.duration = 1.0;
  d.variables = struct();
  dj = NFlow.internal.acausalExpand(jsonencode(d));
end
%=============================================================================
% 1) Linear SISO island (RC + sine): the dae trajectory is the reference and
%    is pinned against the closed-form response so the reference itself
%    cannot rot. v(t) = A/(1+(w tau)^2) (sin wt - w tau cos wt + w tau
%    e^{-t/tau}), w = 2 pi, tau = 0.1.
dj = isl_rc_sine_diagram();
yD = isl_run(dj, '{"solver":"dae"}', 'nfism_y');
t = (0:100)' * 0.01;
w = 2 * pi;
tau = 0.1;
yan = (1 / (1 + (w * tau) ^ 2)) * (sin(w * t) - w * tau * cos(w * t) + w * tau * exp(-t / tau));
assert_istrue(max(abs(yD - yan)) < 2e-3);
% Reduced island under the native explicit solvers: full-order agreement.
y4 = isl_run(dj, '{"solver":"ode4"}', 'nfism_y');
assert_istrue(max(abs(y4 - yD)) < 5e-6);
y45 = isl_run(dj, '{"solver":"ode45"}', 'nfism_y');
assert_istrue(max(abs(y45 - yD)) < 5e-6);
% Default fixed-step loop: fused low-order stepping (input held over the
% step, like any continuous block there): bounded error, NOT a zero trace.
y0 = isl_run(dj, '', 'nfism_y');
assert_istrue(max(abs(y0 - yD)) < 5e-2);
assert_istrue(max(abs(y0)) > 0.5); % the historical bug returned all zeros
y1 = isl_run(dj, '{"solver":"ode1"}', 'nfism_y');
assert_istrue(max(abs(y1 - yD)) < 5e-2);
%=============================================================================
% 2) Constant-source island (no input port -> NOT reduced): the embedded
%    backward-Euler advance. v(t) = 1 - e^{-t/tau}; the t = 0 sample comes
%    from the consistency solve (differential states pinned, algebraic
%    solved), later samples are the BE recurrence: first-order accurate,
%    exact steady state. Historical: ode4 exploded to ~8e6.
d2 = struct();
d2.blocks = {...
  struct('id', 'V1', 'type', 'ConstantVoltage', 'params', struct('V', 1)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 100)), ...
  struct('id', 'C1', 'type', 'Capacitor', 'params', struct('C', 0.001, 'v0', 0)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfism_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'C1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'C1.n', 'VS.n', 'G.p'}})};
d2.sampleTime = 0.01;
d2.duration = 1.0;
d2.variables = struct();
dj2 = NFlow.internal.acausalExpand(jsonencode(d2));
yc = 1 - exp(-t / tau);
ycD = isl_run(dj2, '{"solver":"dae"}', 'nfism_y');
assert_istrue(max(abs(ycD - yc)) < 1e-4);
yc4 = isl_run(dj2, '{"solver":"ode4"}', 'nfism_y');
assert_istrue(abs(yc4(1)) < 1e-9); % consistency solve at t = 0
assert_istrue(max(abs(yc4 - yc)) < 3e-2); % first-order BE bound
assert_istrue(abs(yc4(end) - yc(end)) < 5e-4); % exact steady state
yc0 = isl_run(dj2, '', 'nfism_y');
assert_istrue(max(abs(yc0 - yc)) < 3e-2);
%=============================================================================
% 3) Nonlinear island (series diode, 7b shape): purely algebraic, so the
%    embedded Newton solves the exact node voltages at every sample under
%    every native loop: solver-tolerance agreement with dae.
d3 = struct();
d3.blocks = {...
  struct('id', 'V1', 'type', 'SineVoltage', 'params', struct('Amplitude', 1, 'Frequency', 1, 'Phase', 0)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 100)), ...
  struct('id', 'D1', 'type', 'Diode', 'params', struct('Is', 1e-9, 'Vt', 0.025)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfism_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d3.connections = {struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d3.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p'}}), ...
  struct('pins', {{'R1.n', 'D1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'D1.n', 'VS.n', 'G.p'}})};
d3.sampleTime = 0.01;
d3.duration = 1.0;
d3.variables = struct();
dj3 = NFlow.internal.acausalExpand(jsonencode(d3));
ydD = isl_run(dj3, '{"solver":"dae"}', 'nfism_y');
yd0 = isl_run(dj3, '', 'nfism_y');
assert_istrue(max(abs(yd0 - ydD)) < 1e-5);
yd4 = isl_run(dj3, '{"solver":"ode4"}', 'nfism_y');
assert_istrue(max(abs(yd4 - ydD)) < 1e-5);
yd45 = isl_run(dj3, '{"solver":"ode45"}', 'nfism_y');
assert_istrue(max(abs(yd45 - ydD)) < 1e-5);
%=============================================================================
% 4) Stateful CVODES-session backends: a remaining (non-reduced) island is a
%    typed rejection naming the block and pointing at 'dae', never a wrong
%    trajectory. (The gate runs before backend construction, so it holds
%    whether or not the ode_solvers module is loaded.)
rv = jsondecode(__nflow_simulate__(dj3, '{"solver":"variableNonstiff"}'));
assert_istrue(strcmp(rv.status, 'error'));
assert_istrue(~isempty(strfind(rv.reason, 'algebraic')));
assert_istrue(~isempty(strfind(rv.reason, 'dae')));
assert_istrue(~isempty(strfind(rv.reason, 'physicalIsland')));
%=============================================================================
% 5) NESTED island: a physicalIsland inside a subsystem must integrate exactly
%    like the same island at the top level, under every loop. Two regressions
%    are pinned here: the embedded implicit-step mode must reach the inner
%    context (else the nested trajectory is flat zero), AND the subsystem build
%    must preserve the island's structured `components` netlist (coercing that
%    object array to doubles stripped every component, leaving floating nodes
%    -> a singular island: zero under the explicit loops, IDACalcIC failure
%    under dae). Constant-source RC (0 input -> not reduced, differential
%    capacitor state), v(t) = 1 - e^{-t/tau}, tau = 0.1.
islParams = ['{"domain":"electrical","nodes":3,"components":[', ...
  '{"kind":"vsource","a":1,"b":0,"V":1},', ...
  '{"kind":"resistor","a":1,"b":2,"R":100},', ...
  '{"kind":"capacitor","a":2,"b":0,"C":0.001,"ic":0},', ...
  '{"kind":"voltageSensor","a":2,"b":0,"outPort":0}]}'];
flatJson = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":', islParams, '},', ...
  '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":', ...
  '{"VariableName":"nfism_flat","SaveFormat":"Array","Decimation":1,"MaxDataPoints":"inf"}}],', ...
  '"connections":[{"from":"isl","to":"tw","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":1.0,"variables":{}}'];
innerJson = ['{"blocks":[', ...
  '{"id":"isl","type":"physicalIsland","inputs":0,"outputs":1,"params":', islParams, '},', ...
  '{"id":"yout","type":"labelSink","inputs":1,"outputs":1,"params":{"gotoTag":"y"}}],', ...
  '"connections":[{"from":"isl","to":"yout","fromIndex":0,"toIndex":0}],', ...
  '"externalOutputs":[{"id":"yout","name":"y"}]}'];
nestJson = ['{"blocks":[', ...
  '{"id":"sub","type":"subsystem","inputs":0,"outputs":1,"params":{"subsystem":', innerJson, '}},', ...
  '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":', ...
  '{"VariableName":"nfism_nest","SaveFormat":"Array","Decimation":1,"MaxDataPoints":"inf"}}],', ...
  '"connections":[{"from":"sub","to":"tw","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":1.0,"variables":{}}'];
yc = 1 - exp(-t / tau);
% dae works through the subsystem (components preserved -> non-singular).
rnD = jsondecode(__nflow_simulate__(nestJson, '{"solver":"dae"}'));
assert_istrue(strcmp(rnD.status, 'done'));
ynD = evalin('base', 'nfism_nest'); ynD = ynD(:);
assert_istrue(max(abs(ynD - yc)) < 1e-4);
% Nested embedded implicit step is byte-identical to the flat one.
jsondecode(__nflow_simulate__(flatJson, '{"solver":"ode4"}'));
yf4 = evalin('base', 'nfism_flat'); yf4 = yf4(:);
rn4 = jsondecode(__nflow_simulate__(nestJson, '{"solver":"ode4"}'));
assert_istrue(strcmp(rn4.status, 'done'));
yn4 = evalin('base', 'nfism_nest'); yn4 = yn4(:);
assert_istrue(numel(yn4) == numel(yf4));
assert_istrue(max(abs(yn4 - yf4)) < 1e-9);
assert_istrue(max(abs(yn4 - yc)) < 3e-2); % first-order BE bound
assert_istrue(abs(yn4(end) - yc(end)) < 5e-4); % exact steady state
% Default fixed-step loop through the subsystem: bounded, not a zero trace.
rn0 = jsondecode(__nflow_simulate__(nestJson, ''));
assert_istrue(strcmp(rn0.status, 'done'));
yn0 = evalin('base', 'nfism_nest'); yn0 = yn0(:);
assert_istrue(max(abs(yn0)) > 0.5);
assert_istrue(max(abs(yn0 - yc)) < 3e-2);
%=============================================================================
