%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% The adaptive ode45 backend must honour the diagram's relTol / absTol. These
% travel diagram JSON -> SimEngineSolverLoop (sopts) -> makeSolverBackend ->
% DormandPrinceBackend -- the same fields the GUI's tolerance inputs serialize
% (state.relTol/absTol -> serializeDiagram). This locks that wiring end to end.
%
% Harmonic oscillator x'' = -w^2 x, w = 2*pi (period 1 s), x(0)=1, x'(0)=0 ->
% x(t) = cos(w t). Over 10 periods the accumulated phase/amplitude error is
% strongly tolerance-sensitive. With the SAME communication step, only the
% tolerance differs: a tight tolerance forces the adaptive controller to
% subdivide and track cos to ~1e-10; a loose tolerance accepts bigger substeps
% and drifts by ~1e-3. If the tolerance were silently ignored the two runs would
% be identical.
%=============================================================================
function y = run_tol(relTol, absTol)
  w = 2 * pi;
  m = struct();
  m.solver = 'ode45';
  m.relTol = relTol;
  m.absTol = absTol;
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
  m.sampleTime = 0.1;
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
yTight = run_tol(1e-11, 1e-12);
yLoose = run_tol(1e-2, 1e-2);
errTight = abs(yTight - target);
errLoose = abs(yLoose - target);
%=============================================================================
% A tight tolerance tracks the analytic solution over 10 periods.
assert_istrue(errTight < 1e-8);
% Same communication step, looser tolerance -> measurably worse (the backend
% reads the tolerance; if it were ignored, errLoose would equal errTight).
assert_istrue(errLoose > 1e-5);
% ... but still tracking the solution, not diverging.
assert_istrue(errLoose < 1e-1);
% Explicit ordering: loosening the tolerance strictly degrades accuracy.
assert_istrue(errLoose > errTight * 100);
%=============================================================================
