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
% For / While Iterator subsystems. The body runs several times
% sequentially within one step, accumulating inner state across iterations,
% with the iteration index exposed by an iteratorNumber block. Inner state is
% re-seeded at each step entry, so the result is the same every step.
%=============================================================================
% Shared accumulator body: out = sum(iteration index over the iterations),
% i.e. a unit-delay accumulator fed by (delay + iteratorNumber).
acc_blocks = {struct('id', 'inum', 'type', 'iteratorNumber', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'acc', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '++')), ...
  struct('id', 'del', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
acc_conns = {struct('from', 'inum', 'to', 'acc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'del', 'to', 'acc', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'acc', 'to', 'del', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'acc', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
%=============================================================================
% For Iterator, fixed IterationLimit = 5: sum 1..5 = 15 every step.
evalin('base', 'clear(''for_out'')');
inner = struct();
inner.blocks = acc_blocks;
inner.connections = acc_conns;
inner.externalOutputs = {struct('id', 'sout', 'name', 'y')};
d = struct();
d.blocks = {struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'params', ...
    struct('subsystem', inner, 'controlPort', struct('kind', 'forIterator', 'IterationSource', 'internal', 'IterationLimit', 5))), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'for_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.5; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
asserts.isequal(r.status, 'done');
v = evalin('base', 'for_out'); v = v(:);
asserts.istrue(numel(v) >= 2);
for k = 1:numel(v)
  asserts.isapprox(v(k), 15.0, 0, 1e-9);
end
evalin('base', 'clear(''for_out'')');
%=============================================================================
% For Iterator, EXTERNAL count from an input port (constant 3): sum 1..3 = 6.
evalin('base', 'clear(''fore_out'')');
inner2 = struct();
inner2.blocks = acc_blocks;
inner2.connections = acc_conns;
inner2.externalOutputs = {struct('id', 'sout', 'name', 'y')};
de = struct();
de.blocks = {struct('id', 'n', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, 'params', ...
    struct('subsystem', inner2, 'controlPort', struct('kind', 'forIterator', 'IterationSource', 'external'))), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'fore_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
de.connections = {struct('from', 'n', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
de.sampleTime = 0.1; de.duration = 0.3; de.variables = struct();
re = jsondecode(__nflow_simulate__(jsonencode(de)));
asserts.isequal(re.status, 'done');
ve = evalin('base', 'fore_out'); ve = ve(:);
for k = 1:numel(ve)
  asserts.isapprox(ve(k), 6.0, 0, 1e-9);
end
evalin('base', 'clear(''fore_out'')');
%=============================================================================
% While Iterator, condition-terminated (do-while). The loop continues while the
% running accumulator is below 10: 1, 3, 6, 10 -> stops after the 4th iteration
% (10 is no longer < 10), so the output is 10 every step. The continue predicate
% is (acc < 10), built as not(acc >= 10) so it needs no operator parameter.
evalin('base', 'clear(''wh_out'')');
winner = struct();
winner.blocks = {struct('id', 'inum', 'type', 'iteratorNumber', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'acc', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '++')), ...
  struct('id', 'del', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'cmp', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, 'params', struct('const', 10)), ...
  struct('id', 'inv', 'type', 'not', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'cnd', 'type', 'iteratorCondition', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
winner.connections = {struct('from', 'inum', 'to', 'acc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'del', 'to', 'acc', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'acc', 'to', 'del', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'acc', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cmp', 'to', 'inv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'inv', 'to', 'cnd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'acc', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
winner.externalOutputs = {struct('id', 'sout', 'name', 'y')};
dw = struct();
dw.blocks = {struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'params', ...
    struct('subsystem', winner, 'controlPort', struct('kind', 'whileIterator', 'MaxIterations', 100))), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'wh_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dw.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dw.sampleTime = 0.1; dw.duration = 0.3; dw.variables = struct();
rw = jsondecode(__nflow_simulate__(jsonencode(dw)));
asserts.isequal(rw.status, 'done');
vw = evalin('base', 'wh_out'); vw = vw(:);
for k = 1:numel(vw)
  asserts.isapprox(vw(k), 10.0, 0, 1e-9);
end
evalin('base', 'clear(''wh_out'')');
%=============================================================================
% While Iterator, cap-terminated. With a condition that never becomes zero
% (constant 1), the MaxIterations = 3 safety cap stops the loop after 3
% iterations, so the accumulator output is 1 + 2 + 3 = 6 every step.
evalin('base', 'clear(''whc_out'')');
cinner = struct();
cinner.blocks = {struct('id', 'inum', 'type', 'iteratorNumber', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'acc', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '++')), ...
  struct('id', 'del', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'cnd', 'type', 'iteratorCondition', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
cinner.connections = {struct('from', 'inum', 'to', 'acc', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'del', 'to', 'acc', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'acc', 'to', 'del', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'one', 'to', 'cnd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'acc', 'to', 'sout', 'fromIndex', 0, 'toIndex', 0)};
cinner.externalOutputs = {struct('id', 'sout', 'name', 'y')};
dc = struct();
dc.blocks = {struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'params', ...
    struct('subsystem', cinner, 'controlPort', struct('kind', 'whileIterator', 'MaxIterations', 3))), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'whc_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dc.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dc.sampleTime = 0.1; dc.duration = 0.2; dc.variables = struct();
rc = jsondecode(__nflow_simulate__(jsonencode(dc)));
asserts.isequal(rc.status, 'done');
vc = evalin('base', 'whc_out'); vc = vc(:);
for k = 1:numel(vc)
  asserts.isapprox(vc(k), 6.0, 0, 1e-9);
end
evalin('base', 'clear(''whc_out'')');
%=============================================================================
% An iterator subsystem runs its body multiple times per step and owns its state
% advance, so a solver-integrated (continuous) inner state is rejected with a
% clear diagnostic rather than integrated meaninglessly.
rej = struct();
rej.blocks = {struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct('InitialCondition', 0)), ...
  struct('id', 'yo', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, 'params', struct('gotoTag', 'y'))};
rej.connections = {struct('from', 'one', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'yo', 'fromIndex', 0, 'toIndex', 0)};
rej.externalOutputs = {struct('id', 'yo', 'name', 'y')};
dr = struct();
dr.blocks = {struct('id', 'sub', 'type', 'subsystem', 'inputs', 0, 'outputs', 1, 'params', ...
    struct('subsystem', rej, 'controlPort', struct('kind', 'forIterator', 'IterationSource', 'internal', 'IterationLimit', 3))), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', ...
    struct('VariableName', 'rej_out', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dr.connections = {struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dr.sampleTime = 0.1; dr.duration = 0.2; dr.variables = struct();
rr = jsondecode(__nflow_simulate__(jsonencode(dr)));
asserts.isequal(rr.status, 'error');
asserts.istrue(~isempty(strfind(lower(rr.reason), 'continuous')));
asserts.istrue(~isempty(strfind(lower(rr.reason), 'iterator')));
%=============================================================================
