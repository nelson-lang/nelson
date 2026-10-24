%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% trim: find a steady state f(x, u0) = 0 by Newton on the state
% Jacobian A from linmod.
%=============================================================================
function f = write_model(ssblock)
  blocks = {...
    struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'u', 'isExternalPort', true)), ...
    ssblock, ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, ...
      'params', struct('label', 'y'))};
  connections = {...
    struct('from', 'u', 'to', 'ss', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ss', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
  d = struct('blocks', {blocks}, 'connections', {connections}, ...
    'sampleTime', 0.01, 'runtime', 1.0, 'variables', struct());
  f = [tempname(), '.nflow'];
  fid = fopen(f, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
end
%=============================================================================
% Scalar: A=-2, B=1, u0=2 -> equilibrium x* = -A^-1 B u0 = 1, xdot = 0 there.
f = write_model(struct('id', 'ss', 'type', 'stateSpace', 'inputs', 1, 'outputs', 1, ...
  'params', struct('A', -2, 'B', 1, 'C', 1, 'D', 0.5)));
[x, u, y, dx] = trim(f, 0, 2);
assert_istrue(abs(x - 1) < 1e-6);
assert_istrue(abs(u - 2) < 1e-12);
assert_istrue(abs(y - 2) < 1e-6); % y = C*x + D*u = 1 + 1 = 2
assert_istrue(norm(dx) < 1e-8);
%=============================================================================
% 2x2: A=[0 1;-2 -3], B=[0;1], u0=3 -> A x* = -B u0 -> x* = [1.5; 0].
f = write_model(struct('id', 'ss', 'type', 'stateSpace', 'inputs', 1, 'outputs', 1, ...
  'params', struct('A', {{0, 1, -2, -3}}, 'B', {{0, 1}}, 'C', {{1, 0}}, 'D', 0)));
[x2, ~, ~, dx2] = trim(f, [0; 0], 3);
assert_istrue(max(abs(x2 - [1.5; 0])) < 1e-6);
assert_istrue(norm(dx2) < 1e-8);
%=============================================================================
% Nonlinear plant x' = u - x^2 (equilibrium x = sqrt(u0)): the Newton iteration
% must actually converge (the linear cases above solve in one step). At u0 = 4
% the equilibrium is x = 2, and linmod there gives the local Jacobian A = -2x.
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'u', 'isExternalPort', true)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sq', 'type', 'mult', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'ng', 'type', 'negate', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('label', 'y'))};
d.connections = {...
  struct('from', 'ig', 'to', 'sq', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'sq', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sq', 'to', 'ng', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ng', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01;
d.runtime = 1.0;
d.variables = struct();
fnl = [tempname(), '.nflow'];
fid = fopen(fnl, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
[xnl, ~, ~, dxnl] = trim(fnl, 1, 4);
assert_istrue(abs(xnl - 2) < 1e-6);
assert_istrue(norm(dxnl) < 1e-8);
Anl = linmod(fnl, xnl, 4);
assert_istrue(abs(Anl - (-4)) < 1e-5); % A = -2*x = -4 at the equilibrium
%=============================================================================

