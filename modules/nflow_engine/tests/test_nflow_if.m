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
% L3: if block. A restricted boolean expression over the inputs u1..un selects
% the first true clause; its output is 1.0 and the others 0.0. Property names and
% syntax (IfExpression, ElseIfExpressions comma-separated, ShowElse on/off) mirror
% the reference tool.
%=============================================================================

function y = run_if(u1, u2, port)
  d = struct();
  d.blocks = {...
    struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', u1)), ...
    struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', u2)), ...
    struct('id', 'iff', 'type', 'if', 'inputs', 2, 'outputs', 3, ...
      'params', struct('IfExpression', 'u1 > 0.5', ...
      'ElseIfExpressions', 'u2 > 0.5', 'ShowElse', 'on')), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'if_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'a', 'to', 'iff', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'b', 'to', 'iff', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'iff', 'to', 'tw', 'fromIndex', port, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert(strcmp(r.status, 'done'));
  o = evalin('base', 'if_out');
  y = o(end);
end

evalin('base', 'clear(''if_out'')');
% u1 > 0.5 -> if clause (port 0)
asserts.isapprox(run_if(1, 0, 0), 1.0, 0, 1e-12);
asserts.isapprox(run_if(1, 0, 1), 0.0, 0, 1e-12);
% u1 not, u2 > 0.5 -> elseif clause (port 1)
asserts.isapprox(run_if(0, 1, 1), 1.0, 0, 1e-12);
asserts.isapprox(run_if(0, 1, 0), 0.0, 0, 1e-12);
% neither -> else clause (port 2)
asserts.isapprox(run_if(0, 0, 2), 1.0, 0, 1e-12);
asserts.isapprox(run_if(0, 0, 0), 0.0, 0, 1e-12);
% both true -> if wins (first true clause)
asserts.isapprox(run_if(1, 1, 0), 1.0, 0, 1e-12);
asserts.isapprox(run_if(1, 1, 1), 0.0, 0, 1e-12);
evalin('base', 'clear(''if_out'')');
%=============================================================================
