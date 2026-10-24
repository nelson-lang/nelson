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
% <--WEIGHT 64-->
% <--TIMEOUT 600-->
%=============================================================================
% A function-call subsystem, generated - when the call can be expressed.
%
% The whole family used to be refused on the grounds that a caller-driven
% schedule has no straight-line lowering. That is true of a generator firing
% SEVERAL times per step: those calls are a loop within the step, carrying state
% from one to the next, while the flattener inlines a body once. It is not true
% of a generator firing ONCE per step, which is simply a subsystem run where the
% diagram's order puts it - and that is the common case.
%
% So the refusal keys on the invocation COUNT now, and the boundary was measured
% rather than assumed: a three-call accumulator, let through, generated code
% that added 1 per step where the simulation added 3.
%=============================================================================
function d = fc_doc(varName, iters)
  inner = struct();
  inner.blocks = {...
    struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'in1')), ...
    struct('id', 'dl', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'ad', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
      'params', struct('Inputs', '++')), ...
    struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('label', 'out1'))};
  inner.connections = {...
    struct('from', 'in1', 'to', 'ad', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'dl', 'to', 'ad', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'ad', 'to', 'dl', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ad', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
  inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
  inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
  inner.controlPort = struct('kind', 'functionCall');
  d = struct();
  d.blocks = {...
    struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', 1)), ...
    struct('id', 'g', 'type', 'functionCallGenerator', 'inputs', 0, 'outputs', 1, ...
      'params', struct('NumberOfIterations', iters)), ...
    struct('id', 's', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
      'params', struct('subsystem', inner, ...
        'controlPort', struct('kind', 'functionCall'))), ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', varName, 'SaveFormat', 'Array', ...
        'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 's', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1;
  d.duration = 0.5;
  d.variables = struct();
end
%=============================================================================
% One call per step: the accumulator counts the steps.
d1 = fc_doc('nffc_1', 1);
r1 = jsondecode(__nflow_simulate__(jsonencode(d1)));
assert_isequal(r1.status, 'done');
y1 = evalin('base', 'nffc_1'); y1 = y1(:)';
assert_istrue(max(abs(y1(1:5) - [1 2 3 4 5])) < 1e-9);
evalin('base', 'clear(''nffc_1'')');
f1 = [tempname(), '.nflow'];
[~, stem1] = fileparts(f1);
fid = fopen(f1, 'wt');
fwrite(fid, jsonencode(d1));
fclose(fid);
nflow_codegenerate(f1, tempdir(), false, 'C');
src1 = fileread([tempdir(), stem1, '/model.c']);
assert_istrue(isempty(strfind(src1, 'unknown:')));
rmfile(f1);
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d1, 'nffc_1n', 'C', {'y'}, {'nffc_1'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d1, 'nffc_1n', 'rust', {'y'}, {'nffc_1'}, 5e-6, 1e-9);
  end
end
%=============================================================================
% Three calls per step: the simulation counts three, and code generation says
% so rather than emitting one.
d3 = fc_doc('nffc_3', 3);
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
y3 = evalin('base', 'nffc_3'); y3 = y3(:)';
assert_istrue(max(abs(y3(1:5) - [3 6 9 12 15])) < 1e-9);
evalin('base', 'clear(''nffc_3'')');
f3 = [tempname(), '.nflow'];
fid = fopen(f3, 'wt');
fwrite(fid, jsonencode(d3));
fclose(fid);
msg3 = '';
try
  nflow_codegenerate(f3, tempdir(), false, 'C');
catch e3
  msg3 = e3.message;
end
rmfile(f3);
assert_istrue(~isempty(strfind(lower(msg3), 'conditional')));
%=============================================================================
