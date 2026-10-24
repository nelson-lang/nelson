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
% The pid derivative term used to be ideal and nothing else: one sample of
% D/dt on a step, which is a spike whose size depends on the step size rather
% than on the model. A filter coefficient N makes that term D N s / (s + N),
% so a step produces D N exp(-N (t - t0)) - an answer the model owns.
%
% N is optional: without it the block keeps the ideal derivative it always had.
%=============================================================================
function d = pid_doc(D, N, solver, dt)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'pid_filter';
  d.runtime = 1.5;
  d.sampleTime = dt;
  if ~isempty(solver)
    d.solver = solver;
  end
  p = struct('P', 0, 'I', 0, 'D', D);
  if N > 0
    p.N = N;
  end
  d.blocks = {...
    struct('id', 'u', 'type', 'step', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Time', 0.5)), ...
    struct('id', 'c', 'type', 'pid', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', p), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_pf', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'c', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
end
function y = pid_run(d)
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = [out.tout(:), out.y_pf(:)];
  evalin('base', 'clear(''y_pf'')');
end
%=============================================================================
% Under a solver, a filtered derivative of a unit step decays as D N exp(-N t)
% from the step instant. D = 1, N = 10: it starts at 10 and is down to
% 10 exp(-5) by half a second later.
D = 1; N = 10;
y = pid_run(pid_doc(D, N, 'ode4', 0.001));
t = y(:, 1);
v = y(:, 2);
idx = @(tt) find(abs(t - tt) < 1e-9, 1);

% Nothing before the step.
assert_istrue(abs(v(idx(0.4))) < 1e-12);
% The filtered peak is D*N, not D/dt: with dt = 0.001 an ideal derivative would
% have answered 1000 here. It is a shade under D*N because the step is crossed
% rather than stopped at, so the last stage of the step ending at 0.5 has
% already moved the filter state by N*D*dt/6.
assert_isapprox(v(idx(0.5)), D * N * (1.0 - N * 0.001 / 6), 1e-6, 1e-6);
% and it decays as the filter says it should.
for tt = [0.6, 0.8, 1.0]
  assert_isapprox(v(idx(tt)), D * N * exp(-N * (tt - 0.5)), 5e-3, 5e-3);
end
%=============================================================================
% The step size no longer sets the peak: ten times finer, same answer.
yf = pid_run(pid_doc(D, N, 'ode4', 0.0001));
tf = yf(:, 1);
vf = yf(:, 2);
assert_isapprox(vf(find(abs(tf - 0.5) < 1e-9, 1)), D * N, 1e-6, 1e-6);
%=============================================================================
% The same on the fixed-step path: the filter is advanced by its own equation,
% so the term is read before the state moves and the peak is D N again.
yd = pid_run(pid_doc(D, N, '', 0.001));
td = yd(:, 1);
vd = yd(:, 2);
assert_isapprox(vd(find(abs(td - 0.5) < 1e-9, 1)), D * N, 1e-6, 1e-6);
for tt = [0.6, 0.8]
  assert_isapprox(vd(find(abs(td - tt) < 1e-9, 1)), ...
    D * N * exp(-N * (tt - 0.5)), 2e-2, 2e-2);
end
%=============================================================================
% Without N the block is exactly what it was: one sample of D/dt on the step.
yi = pid_run(pid_doc(D, 0, 'ode4', 0.01));
ti = yi(:, 1);
vi = yi(:, 2);
assert_isapprox(vi(find(abs(ti - 0.5) < 1e-9, 1)), D / 0.01, 1e-6, 1e-6);
assert_istrue(abs(vi(find(abs(ti - 0.6) < 1e-9, 1))) < 1e-9);
%=============================================================================
% A filter coefficient that is not a coefficient (zero, negative, infinite)
% leaves the ideal derivative in place rather than inventing a filter.
for bad = [0, -3, Inf]
  db = pid_doc(D, 1, 'ode4', 0.01);
  db.blocks{2}.params.N = bad;
  yb = pid_run(db);
  tb = yb(:, 1);
  vb = yb(:, 2);
  assert_isapprox(vb(find(abs(tb - 0.5) < 1e-9, 1)), D / 0.01, 1e-6, 1e-6);
end
%=============================================================================
