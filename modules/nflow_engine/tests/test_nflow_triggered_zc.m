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
% Trigger edge localization by zero-crossing. A
% triggered subsystem exposes its control signal as a zero-crossing surface, so
% a solver-loop run stops exactly at the edge and fires the body there
% (Phase::EVENT), instead of latching the edge at the next major step (O(dt)).
% The control is sin(pi t - 0.735 pi): one rising zero at t* = 0.735, between
% two output samples (dt = 0.1). The body passes a ramp (slope 1) through, so
% the held output equals the ramp AT THE FIRE INSTANT: t* under the solver
% (localized by bisection), 0.8 under the fixed-step engine (next sample).
%=============================================================================
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, 'params', struct('label', 'in1')), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('label', 'out1'))};
inner.connections = {struct('from', 'in1', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
inner.controlPort = struct('kind', 'trigger', 'triggerType', 'rising');

function d = build_zc_model(inner, varName, solver)
  d = struct('blocks', {{...
    struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 1)), ...
    struct('id', 'ctl', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Amplitude', 1, 'Frequency', 0.5, 'Phase', -0.735 * pi)), ...
    struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
      'params', struct('subsystem', inner, 'controlPort', inner.controlPort)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', varName, 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf')), ...
    struct('id', 'c2', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
    struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
    struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())}}, ...
    'connections', {{...
      struct('from', 'r', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'ctl', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
      struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'c2', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'ig', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)}}, ...
    'sampleTime', 0.1, 'duration', 1.0, 'solver', solver, 'variables', struct());
end
%=============================================================================
function v = run_zc_get(d, varName)
  evalin('base', ['clear(''', varName, ''')']);
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(r.status, 'done');
  v = evalin('base', varName); v = v(:)';
  evalin('base', ['clear(''', varName, ''')']);
end
%=============================================================================
% Fixed-step engine: the edge is latched at the next major step. The control
% is negative at 0.7 and positive at 0.8, so the body fires at the 0.8 sample
% and holds the ramp value there.
vd = run_zc_get(build_zc_model(inner, 'zc_out', 'discrete'), 'zc_out');
assert_isequal(numel(vd), 11);
assert_istrue(max(abs(vd(1:8))) < 1e-12); % held 0 until the fire
assert_istrue(max(abs(vd(9:11) - 0.8)) < 1e-9); % fired at the 0.8 sample
%=============================================================================
% Solver loop (native ode4 with root localization): the control surface stops
% the advance exactly at the rising zero t* = 0.735 and the body fires there,
% sampling the ramp at the crossing (bisection localizes to ~1e-12).
vo = run_zc_get(build_zc_model(inner, 'zc_out', 'ode4'), 'zc_out');
assert_isequal(numel(vo), 11);
assert_istrue(max(abs(vo(1:8))) < 1e-12); % held 0 until the fire
assert_istrue(max(abs(vo(9:11) - 0.735)) < 1e-6); % fired AT the edge instant
%=============================================================================
% A falling-only trigger ignores the rising crossing entirely (rootDirection):
% same run, no fire anywhere (the control never crosses downward before t=1).
innerF = inner;
innerF.controlPort = struct('kind', 'trigger', 'triggerType', 'falling');
vf = run_zc_get(build_zc_model(innerF, 'zc_out', 'ode4'), 'zc_out');
assert_istrue(max(abs(vf)) < 1e-12);
%=============================================================================
