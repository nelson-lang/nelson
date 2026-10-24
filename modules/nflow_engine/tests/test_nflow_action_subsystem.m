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
% L3: action subsystems driven by an if action selector, recombined by merge.
% clock -> if (u1 < 0.5) -> [ifPort, elsePort]; ifPort gates action subsystem A
% (emits 10), elsePort gates action subsystem B (emits 20). Both feed a merge.
% Result: the branch selected by the if runs each step -> 10 then 20.
%=============================================================================
evalin('base', 'clear(''act_out'')');

function s = action_subsystem(value)
  s = struct();
  s.blocks = {...
    struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', value)), ...
    struct('id', 'yout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('gotoTag', 'y'))};
  s.connections = {struct('from', 'k', 'to', 'yout', 'fromIndex', 0, 'toIndex', 0)};
  s.externalOutputs = {struct('id', 'yout', 'name', 'y')};
  s.controlPort = struct('kind', 'action');
end

ac = struct('kind', 'action');
d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sel', 'type', 'if', 'inputs', 1, 'outputs', 2, ...
    'params', struct('IfExpression', 'u1 < 0.5', 'ElseIfExpressions', '', 'ShowElse', 'on')), ...
  struct('id', 'subA', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, ...
    'params', struct('subsystem', action_subsystem(10), 'controlPort', ac)), ...
  struct('id', 'subB', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, ...
    'params', struct('subsystem', action_subsystem(20), 'controlPort', ac)), ...
  struct('id', 'mrg', 'type', 'merge', 'inputs', 2, 'outputs', 1, ...
    'params', struct('InitialOutput', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'act_out', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sel', 'to', 'subA', 'fromIndex', 0, 'toIndex', 0), ...   % if port -> A
  struct('from', 'sel', 'to', 'subB', 'fromIndex', 1, 'toIndex', 0), ...   % else port -> B
  struct('from', 'subA', 'to', 'mrg', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'subB', 'to', 'mrg', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'mrg', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d)));
asserts.isequal(r.status, 'done');
v = evalin('base', 'act_out');
v = v(:);
asserts.isequal(numel(v), 11);
% t < 0.5 : if selects branch A -> 10.
asserts.isapprox(v(1), 10.0, 0, 1e-9);
asserts.isapprox(v(5), 10.0, 0, 1e-9);
% t >= 0.5 : else selects branch B -> 20.
asserts.isapprox(v(6), 20.0, 0, 1e-9);
asserts.isapprox(v(end), 20.0, 0, 1e-9);

evalin('base', 'clear(''act_out'')');
%=============================================================================
