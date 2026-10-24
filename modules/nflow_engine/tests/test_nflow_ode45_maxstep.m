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
% The adaptive ode45 backend must honour the diagram's maxStep as a CEILING on
% the adaptive step (not merely the initial step). maxStep travels diagram JSON
% -> SimEngineSolverLoop (sopts.maxStep) -> DormandPrinceBackend, which caps the
% step before each stride -- the same field the GUI's "Max step" input feeds.
%
% Harmonic oscillator x'' = -w^2 x, w = 2*pi, x(0)=1, x'(0)=0 -> x(t)=cos(w t).
% With a deliberately coarse communication step (1 s) and a loose tolerance, the
% unconstrained adaptive controller accepts oversized strides and diverges. A
% small maxStep forces fine strides regardless of the loose tolerance, keeping
% the trajectory on cos -- and a still-smaller maxStep is strictly more accurate.
% If maxStep were ignored, all three runs would diverge identically.
%=============================================================================
function y = run_ms(maxStep)
  w = 2 * pi;
  m = struct();
  m.solver = 'ode45';
  m.relTol = 1e-1; % deliberately loose: accuracy must come from the step cap
  m.absTol = 1e-1;
  if maxStep > 0
    m.maxStep = maxStep;
  end
  m.blocks = {...
    struct('id', 'i1', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 1)), ...
    struct('id', 'i2', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Gain', -w * w)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'y'))};
  m.connections = {...
    struct('from', 'i2', 'to', 'i1', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'i2', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  m.sampleTime = 1.0; % coarse communication step
  m.runtime = 10.0;
  m.variables = struct();
  f = [tempname(), '.nflow'];
  fid = fopen(f, 'wt'); fwrite(fid, jsonencode(m)); fclose(fid);
  out = sim(f);
  vals = out.logsout(1).values;
  y = vals(end);
end
%=============================================================================
target = cos(2 * pi * 10.0); % = 1
errNone = abs(run_ms(0.0) - target); % no cap
errCap = abs(run_ms(0.05) - target); % capped at 0.05
errTiny = abs(run_ms(0.02) - target); % capped at 0.02
%=============================================================================
% Without a cap, the loose tolerance over a coarse communication step diverges.
assert_istrue(errNone > 1.0);
% A small maxStep keeps the trajectory on cos despite the loose tolerance
% -> the cap is enforced, not ignored.
assert_istrue(errCap < 1e-3);
% A still-smaller cap is strictly more accurate -> maxStep is a real ceiling.
assert_istrue(errTiny < errCap);
%=============================================================================
