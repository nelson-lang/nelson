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
% Regression: string-valued operator parameters (`relop`, `Operator`) of blocks
% placed INSIDE a subsystem must survive the inner-param resolution. The bug was
% that buildSubsystemInnerState resolved every string param to a double via
% resolveNumeric, which collapses a non-numeric string (e.g. "lt") to 0.0 rather
% than NaN. That finite 0.0 clobbered `relop`/`Operator`, so operatorParam saw a
% number instead of a string and silently fell back to the handler default (ge).
% At the top level the params were read raw and worked, so the bug only bit
% nested blocks. Discovered 2026-08-09 while adding For/While Iterator subsystems.
%=============================================================================
function r = run_sim(d)
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
end
%=============================================================================
% Helper: wrap an inner diagram (blocks + connections, sink id 'sout') in a
% subsystem feeding a toWorkspace variable, run it, and return the samples.
function v = run_subsystem(inner, varName)
  inner.externalOutputs = {struct('id', 'sout', 'name', 'y')};
  d = struct();
  d.blocks = {struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'params', ...
      struct('subsystem', inner)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
      struct('VariableName', varName, 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  evalin('base', ['clear(''', varName, ''')']);
  r = run_sim(d);
  asserts.isequal(r.status, 'done');
  v = evalin('base', varName); v = v(:);
  asserts.istrue(numel(v) >= 1);
  evalin('base', ['clear(''', varName, ''')']);
end
%=============================================================================
% 1. compareToConstant inside a subsystem, relop = 'lt', const = 10, input = 1.
%    Correct: (1 < 10) -> 1. Buggy default 'ge': (1 >= 10) -> 0.
inner = struct();
inner.blocks = {struct('id', 'c1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'cmp', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, 'params', struct('relop', 'lt', 'const', 10)), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
inner.connections = {struct('from', 'c1', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cmp', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
v = run_subsystem(inner, 'sub_c2c_out');
for k = 1:numel(v)
  asserts.isapprox(v(k), 1.0, 0, 1e-12);
end
%=============================================================================
% 2. Control: the SAME compareToConstant at the TOP level already worked.
evalin('base', 'clear(''top_c2c_out'')');
dt = struct();
dt.blocks = {struct('id', 'c1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'cmp', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, 'params', struct('relop', 'lt', 'const', 10)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'top_c2c_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dt.connections = {struct('from', 'c1', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cmp', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
rt = run_sim(dt);
asserts.isequal(rt.status, 'done');
vt = evalin('base', 'top_c2c_out'); vt = vt(:);
for k = 1:numel(vt)
  asserts.isapprox(vt(k), 1.0, 0, 1e-12);
end
evalin('base', 'clear(''top_c2c_out'')');
%=============================================================================
% 3. relationalOperator inside a subsystem, Operator = 'lt', inputs 2 and 5.
%    Correct: (2 < 5) -> 1. Buggy default 'ge': (2 >= 5) -> 0.
inner = struct();
inner.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'rel', 'type', 'relationalOperator', 'inputs', 2, 'outputs', 1, 'params', struct('Operator', 'lt')), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
inner.connections = {struct('from', 'a', 'to', 'rel', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'rel', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'rel', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
v = run_subsystem(inner, 'sub_rel_out');
for k = 1:numel(v)
  asserts.isapprox(v(k), 1.0, 0, 1e-12);
end
%=============================================================================
% 4. compareToZero inside a subsystem, relop = 'lt', input = 5.
%    Correct: (5 < 0) -> 0. Buggy default 'ne': (5 != 0) -> 1.
inner = struct();
inner.blocks = {struct('id', 'c5', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'cz', 'type', 'compareToZero', 'inputs', 1, 'outputs', 1, 'params', struct('relop', 'lt')), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
inner.connections = {struct('from', 'c5', 'to', 'cz', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cz', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
v = run_subsystem(inner, 'sub_c2z_out');
for k = 1:numel(v)
  asserts.isapprox(v(k), 0.0, 0, 1e-12);
end
%=============================================================================
