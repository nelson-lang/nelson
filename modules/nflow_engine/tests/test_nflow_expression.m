%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% expression block (renamed from userFunc, now simulates too): evaluates a
% restricted math expression of u at every step.
%=============================================================================
% u + u
r = run_expr('u + u', 3);
asserts.isequal(r.status, 'done');
asserts.isapprox(expr_last_scalar(r), 6, 0, 1e-10);
%=============================================================================
% pow via ^ (interpreter semantics preserved).
r = run_expr('u^2', 5);
asserts.isequal(r.status, 'done');
asserts.isapprox(expr_last_scalar(r), 25, 0, 1e-10);
%=============================================================================
% math function
r = run_expr('sqrt(u)', 9);
asserts.isequal(r.status, 'done');
asserts.isapprox(expr_last_scalar(r), 3, 0, 1e-10);
%=============================================================================
function r = run_expr(expr, constValue)
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', constValue)), ...
    struct('id', 'e', 'type', 'expression', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Expr', expr)), ...
    struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
  d.connections = {struct('from', 'c', 'to', 'e', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'e', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
end
%=============================================================================
function v = expr_last_scalar(r)
  y = r.scopes(1).series; if iscell(y), y = y{1}; else, y = y(1, :); end
  v = y(end);
end
%=============================================================================
