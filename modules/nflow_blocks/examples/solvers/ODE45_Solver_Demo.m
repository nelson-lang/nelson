%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Variable-step ode45 demo: adaptive Dormand-Prince 5(4) vs fixed-step ode4.
%
% The variable-step 'ode45' solver takes adaptive sub-steps within each
% communication interval, subdividing where the dynamics are fast so accuracy
% is controlled by the relTol/absTol tolerances rather than by the grid. Here a
% harmonic oscillator x'' = -w^2 x (x(0)=1, x'(0)=0 -> x(t)=cos(w t)) is run over
% five periods on a deliberately COARSE communication step. The fixed-step ode4
% takes exactly one RK4 step per interval and accumulates phase error; ode45
% subdivides internally and tracks the analytic solution far more closely.
%
% Select the solver with the model's 'solver' field ('ode4' or 'ode45'); the
% tolerances travel through the 'relTol' / 'absTol' fields (defaults 1e-6 / 1e-8).
%=============================================================================
w = 2 * pi; % angular frequency: period = 1 s
duration = 5.0; % five periods
base = 0.2; % coarse communication step (5 samples per period)

mk = @(solver) struct('blocks', {{...
  struct('id', 'i1', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'i2', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', -w * w)), ...
  struct('id', 'wS', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'y', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))}}, ...
  'connections', {{...
    struct('from', 'i2', 'to', 'i1', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'i2', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i1', 'to', 'wS', 'fromIndex', 0, 'toIndex', 0)}}, ...
  'sampleTime', base, 'duration', duration, 'runtime', duration, ...
  'solver', solver, 'variables', struct());
%=============================================================================
% Fixed-step RK4 (one step per coarse interval).
jsondecode(__nflow_simulate__(jsonencode(mk('ode4'))));
y4 = evalin('base', 'y'); y4 = y4(:);
%=============================================================================
% Adaptive Dormand-Prince (subdivides internally to meet the tolerances).
jsondecode(__nflow_simulate__(jsonencode(mk('ode45'))));
y45 = evalin('base', 'y'); y45 = y45(:);
evalin('base', 'clear(''y'')');
%=============================================================================
t = (0:base:duration)';
exact = cos(w * t);
err4 = max(abs(y4 - exact));
err45 = max(abs(y45 - exact));
disp('Harmonic oscillator over five periods on a coarse communication step:');
fprintf('  max |error vs cos(w t)|   ode4  (fixed)    : %.3e\n', err4);
fprintf('  max |error vs cos(w t)|   ode45 (adaptive) : %.3e\n', err45);
fprintf('  ode45 is %.0fx closer to the analytic solution here.\n', err4 / max(err45, eps));
%=============================================================================
