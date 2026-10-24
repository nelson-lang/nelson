%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Native adaptive solver ode45 (Dormand-Prince 5(4)) over the shared RHS. A
% variable-step explicit method with embedded 4th-order error control; each
% communication step lands exactly on the reporting grid. Validated against
% closed-form solutions (algorithm-independent) and expected to be at least as
% accurate as the fixed-step RK4 (ode4). Foundation for byte-for-byte
% variable-step codegen.
%=============================================================================
function y = run_solver(model, solver)
  model.solver = solver;
  f = [tempname(), '.nflow'];
  fid = fopen(f, 'wt'); fwrite(fid, jsonencode(model)); fclose(fid);
  out = sim(f);
  y = out.logsout(1).values;
end
%=============================================================================
% (1) x' = -x, x(0) = 1  ->  x(t) = e^{-t}. An integrator (IC = 1) with a -1
% feedback gain realises x' = -x.
decay = struct();
decay.blocks = {...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y'))};
decay.connections = {...
  struct('from', 'ig', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
decay.sampleTime = 0.1; decay.runtime = 1.0; decay.variables = struct();

y45 = run_solver(decay, 'ode45');
y4 = run_solver(decay, 'ode4');
% ode45 matches the analytic decay to a tight tolerance.
assert_istrue(abs(y45(end) - exp(-1)) < 1e-7);
% and is at least as accurate as the fixed-step RK4 on the same grid.
assert_istrue(abs(y45(end) - exp(-1)) <= abs(y4(end) - exp(-1)) + 1e-12);
%=============================================================================
% (2) Harmonic oscillator: x' = v (IC 1), v' = -x (IC 0)  ->  x(t) = cos(t).
% Two integrators + a -1 feedback gain. Exercises the multi-state adaptive step.
osc = struct();
osc.blocks = {...
  struct('id', 'i1', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'i2', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y'))};
osc.connections = {...
  struct('from', 'i2', 'to', 'i1', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'i2', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
osc.sampleTime = 0.05; osc.runtime = 3.0; osc.variables = struct();

yo45 = run_solver(osc, 'ode45');
yo4 = run_solver(osc, 'ode4');
assert_istrue(abs(yo45(end) - cos(3)) < 1e-6);
assert_istrue(abs(yo45(end) - cos(3)) <= abs(yo4(end) - cos(3)) + 1e-12);
%=============================================================================
% (3) Zero-crossing: a saturated integrator. x' = 2 (constant input), upper
% saturation limit 1, x(0) = 0  ->  x(t) = min(2t, 1), engaging the limit at
% t = 0.5. Exercises the adaptive backend's sign-change root localisation (the
% integrator declares a ZERO_CROSSING at an engaged limit) -- the state must
% settle exactly at the limit, no overshoot past it.
sat = struct();
sat.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'UpperSaturationLimit', 1)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y'))};
sat.connections = {...
  struct('from', 'c', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
sat.sampleTime = 0.1; sat.runtime = 1.0; sat.variables = struct();

ys45 = run_solver(sat, 'ode45');
assert_istrue(abs(ys45(end) - 1.0) < 1e-6); % settles at the limit
assert_istrue(max(ys45) <= 1.0 + 1e-6); % no overshoot past it
assert_istrue(abs(ys45(3) - 0.4) < 1e-4); % linear ramp before saturation (t=0.2)
%=============================================================================
