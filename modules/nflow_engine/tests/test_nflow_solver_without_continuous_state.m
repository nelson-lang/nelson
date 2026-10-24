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
% Asking for a solver on a diagram that turns out to be purely discrete used to
% be an error, whichever solver it was. For a fixed-step RK stepper that was
% needless: with an empty state vector every stage is a no-op and the discrete
% blocks advance once per step either way, so the fixed-step loop produces the
% same trajectory, to the bit. It now runs.
%
% A variable-step solver keeps the refusal, and for a reason: what it would add
% is event instants located between samples, and locating them needs something
% to integrate.
%=============================================================================
function [ok, msg, y] = run_with(solver)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'solver_without_state';
  % The engine floors a duration at ten base steps, so ask for the ten.
  d.runtime = 1.0;
  d.sampleTime = 0.1;
  if ~isempty(solver)
    d.solver = solver;
  end
  d.blocks = {...
    struct('id', 'r', 'type', 'ramp', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('slope', 1)), ...
    struct('id', 'u', 'type', 'unitDelay', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_ns', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'r', 'to', 'u', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'u', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  ok = true;
  msg = '';
  y = [];
  try
    out = sim(model);
    y = out.y_ns(:)';
    evalin('base', 'clear(''y_ns'')');
  catch e
    ok = false;
    msg = e.message;
  end
  rmfile(model);
end
%=============================================================================
% The fixed-step loop is the reference answer here: a unit delay on a ramp.
[okd, ~, yd] = run_with('');
assert_istrue(okd);
assert_isequal(numel(yd), 11);
%=============================================================================
% Both fixed-step RK steppers now run, and give that same answer exactly.
for s = {'ode1', 'ode4'}
  [ok, msg, y] = run_with(s{1});
  assert_istrue(ok);
  assert_isequal(numel(y), numel(yd));
  assert_istrue(max(abs(y - yd)) < 1e-12);
end
%=============================================================================
% A variable-step solver still says no, and says what it is missing.
[okv, msgv] = run_with('ode45');
assert_isfalse(okv);
assert_istrue(~isempty(strfind(msgv, 'continuous states')));
%=============================================================================
% And a diagram that does have something to integrate is untouched: ode4 runs
% it through the solver loop as before.
di = struct();
di.version = '1.0.0';
di.format = 'nflow-2';
di.name = 'solver_with_state';
di.runtime = 1.0;
di.sampleTime = 0.01;
di.solver = 'ode4';
di.blocks = {...
  struct('id', 'c', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 2)), ...
  struct('id', 'i', 'type', 'integrator', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_ws', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
di.connections = {...
  struct('from', 'c', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
m2 = [tempname(), '.nflow'];
fid = fopen(m2, 'wt');
fwrite(fid, jsonencode(di));
fclose(fid);
o2 = sim(m2);
rmfile(m2);
assert_isapprox(o2.y_ws(end), 2.0, 1e-9, 1e-9);
evalin('base', 'clear(''y_ws'')');
%=============================================================================
