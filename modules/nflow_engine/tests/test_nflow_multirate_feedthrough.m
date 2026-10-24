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
% Sub-rate FEEDTHROUGH sample-and-hold.
%
% A feedthrough block (gain) computes in the ALGEBRAIC phase, so it used to be
% rejected with "no state to hold" when given a SampleTime. It now latches its
% output at each rate hit and replays it between hits (a between-hits sub-rate
% feedthrough block is a constant source).
%=============================================================================

%=============================================================================
% Case 1: a base-rate ramp (slope 1, dt=0.1) drives a gain (K=2) sampled at
% SampleTime=0.3 => rate 3. The gain samples the ramp at t=0,0.3,0.6,0.9 and
% holds between hits: output = 2 * ramp(hit time), a staircase.
%=============================================================================
evalin('base', 'clear(''mr_ft'')');
d = struct();
d.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2, 'SampleTime', 0.3)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'mr_ft', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'r', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done'); % no "no state to hold" error any more
y = evalin('base', 'mr_ft'); y = y(:)';
% ramp(0,0.3,0.6,0.9) = 0,0.3,0.6,0.9 ; gain 2 => 0,0.6,1.2,1.8 held over 3 steps.
yexp = [0 0 0 0.6 0.6 0.6 1.2 1.2 1.2 1.8 1.8];
assert_isequal(numel(y), numel(yexp));
assert_istrue(max(abs(y - yexp)) < 1e-9);
evalin('base', 'clear(''mr_ft'')');

%=============================================================================
% Case 2: a base-rate feedthrough (rate 1) is byte-identical: same model with
% no SampleTime tracks the ramp exactly (no staircase).
%=============================================================================
evalin('base', 'clear(''mr_ft'')');
d2 = d;
d2.blocks{2} = struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
  'params', struct('Gain', 2));
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'mr_ft'); y2 = y2(:)';
t = 0:0.1:1.0;
assert_istrue(max(abs(y2 - 2 * t)) < 1e-9); % exact ramp tracking, no hold
evalin('base', 'clear(''mr_ft'')');

%=============================================================================
% Case 3: sub-rate feedthrough INSIDE an algebraic loop converges and freezes.
%   s = u - g ,  g = held_at_rate3( 0.5 * s ).
% Between hits g is a constant source, so the loop degenerates to s = u - g_held
% and still converges; at a hit g re-samples 0.5*s. Just assert it runs to
% completion (the SCC solver must not stall on the frozen edge).
%=============================================================================
evalin('base', 'clear(''mr_loop'')');
dL = struct();
dL.blocks = {...
  struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 3)), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
    'params', struct('Inputs', '+-')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 0.5, 'SampleTime', 0.3)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'mr_loop', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
dL.connections = {...
  struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
dL.sampleTime = 0.1; dL.duration = 0.6; dL.solver = 'discrete'; dL.variables = struct();
rL = jsondecode(__nflow_simulate__(jsonencode(dL)));
assert_isequal(rL.status, 'done'); % loop with a frozen sub-rate edge still solves
evalin('base', 'clear(''mr_loop'')');

%=============================================================================
% Case 4: the SAME sub-rate feedthrough gain under the global solver path
% (solver='ode4', which routes through SimEngineSolverLoop). An integrator gives
% ode4 a continuous state to integrate (its output is unused); the gain's
% recorded output must be the identical staircase as the discrete path, proving
% the solver record-path ALGEBRAIC gate works.
%=============================================================================
evalin('base', 'clear(''mr_ft'')');
d4 = struct();
d4.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, ...
    'params', struct('slope', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2, 'SampleTime', 0.3)), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'mr_ft', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d4.connections = {...
  struct('from', 'r', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d4.sampleTime = 0.1; d4.duration = 1.0; d4.solver = 'ode4'; d4.variables = struct();
r4 = jsondecode(__nflow_simulate__(jsonencode(d4)));
assert_isequal(r4.status, 'done');
y4 = evalin('base', 'mr_ft'); y4 = y4(:)';
yexp4 = [0 0 0 0.6 0.6 0.6 1.2 1.2 1.2 1.8 1.8];
assert_isequal(numel(y4), numel(yexp4));
assert_istrue(max(abs(y4 - yexp4)) < 1e-9);
evalin('base', 'clear(''mr_ft'')');

%=============================================================================
% Case 5: same model under a real VARIABLE-STEP solver (variableNonstiff,
% CVODES Adams). The RHS re-runs the algebraic solve at CVODES internal steps
% and the record path samples at the output cadence; the recorded gain must be
% the identical staircase, proving the gate holds on the CVODES path too.
%=============================================================================
evalin('base', 'clear(''mr_ft'')');
d5 = d4;
d5.solver = 'variableNonstiff';
r5 = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r5.status, 'done');
y5 = evalin('base', 'mr_ft'); y5 = y5(:)';
assert_isequal(numel(y5), numel(yexp4));
assert_istrue(max(abs(y5 - yexp4)) < 1e-9);
evalin('base', 'clear(''mr_ft'')');
%=============================================================================
