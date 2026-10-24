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
% Regression: a resettable subsystem re-seeded the states held by its inner
% blocks, but under a solver an inner integrator's state lives in the global
% continuous vector, which nothing wrote back. The re-seeded value was
% discarded on the next step and the reset did nothing at all - while the
% fixed-step engine, which has no global vector, reset correctly. The two
% engines now answer the same thing.
%=============================================================================
% A constant 1 integrated inside a resettable subsystem, reset on the rising
% edges of a 0/1 pulse of period 1.5. With dt = 0.1 the integral climbs to 1.4
% and snaps back to 0 at t = 1.5 and t = 3.0.
%=============================================================================
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 'r', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 1, 'Period', 1.5, 'Width', 50, ...
      'StartTime', 0, 'Offset', 0)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', inner, ...
      'controlPort', struct('kind', 'reset', 'resetTriggerType', 'rising'))), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrs_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'u', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'r', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 3.0;
d.variables = struct();
%=============================================================================
expected = [0:0.1:1.4, 0:0.1:1.4, 0];
% Fixed-step: unchanged, it never had a global state vector to miss.
rd = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(rd.status, 'done');
yd = evalin('base', 'nfrs_y'); yd = yd(:)';
assert_isequal(numel(yd), numel(expected));
assert_istrue(max(abs(yd - expected)) < 1e-9);
evalin('base', 'clear(''nfrs_y'')');
%=============================================================================
% Under a solver the integral used to climb straight through both resets.
do = d;
do.solver = 'ode4';
ro = jsondecode(__nflow_simulate__(jsonencode(do)));
assert_isequal(ro.status, 'done');
yo = evalin('base', 'nfrs_y'); yo = yo(:)';
assert_isequal(numel(yo), numel(expected));
assert_istrue(max(abs(yo - expected)) < 1e-9);
assert_istrue(max(abs(yo - yd)) < 1e-9); % the two engines agree
evalin('base', 'clear(''nfrs_y'')');
%=============================================================================
% The generated code had the very same hole, one layer up - its RK4 stepper
% integrates a packed vector too - and used to refuse this model rather than
% emit something that would not reset. It lowers now: this asserts only that it
% is accepted, the sample-by-sample comparison with the simulation living in
% test_nf_codegenerate_cond_reset_ode4.m.
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(do));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
rmfile(f);
%=============================================================================
