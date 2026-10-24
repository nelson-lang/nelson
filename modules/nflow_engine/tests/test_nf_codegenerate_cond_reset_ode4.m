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
% A reset subsystem holding a CONTINUOUS state, under ode4. It used to be
% refused by the generator, and the reason was real: the re-seed writes the
% member's state field, while the RK4 stepper integrates a PACKED state vector
% gathered before the reporting evaluation - so the re-seeded value was read
% back over and the reset did nothing at all. (The simulator had the very same
% hole, one level down: its inner states live in the global vector too.)
%
% The stepper now reads the packed state again after the reporting evaluation,
% which is where a reset fires, and evaluates the first stage from it. Two more
% things had to line up for that to be worth anything: the gate's control is
% latched ONCE per step (a stage-time latch leaves the control of the LAST
% stage, which is the next step's own sample, so no edge is ever seen again),
% and the generated runner's sample instants are k*dt rather than an
% accumulated sum, or the sample carrying the edge lands on the wrong side of
% it.
%
% Model: constant 1 -> [reset: integrator] -> out, reset = a 0.4 s pulse, so
% the state ramps for eight steps and restarts at every rising edge.
%=============================================================================
innerR = struct();
innerR.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
innerR.connections = {...
  struct('from', 'in1', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
innerR.externalInputs = {struct('id', 'in1', 'name', 'in1')};
innerR.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
innerR.controlPort = struct('kind', 'reset');
dr = struct();
dr.blocks = {...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 'p', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 1, 'Period', 0.4, 'PulseWidth', 50, 'PhaseDelay', 0)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', innerR, ...
      'controlPort', struct('kind', 'reset', 'resetTriggerType', 'rising'))), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrk_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
dr.connections = {...
  struct('from', 'one', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'p', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dr.sampleTime = 0.05;
dr.duration = 1.0;
dr.solver = 'ode4';
dr.variables = struct();
%=============================================================================
rs = jsondecode(__nflow_simulate__(jsonencode(dr)));
assert_isequal(rs.status, 'done');
yr = evalin('base', 'nfrk_y');
% Eight steps of 0.05 up to 0.35, then the rising edge at 0.40 restarts it.
assert_istrue(abs(yr(8) - 0.35) < 1e-9);
assert_istrue(abs(yr(9)) < 1e-12);
assert_istrue(abs(yr(10) - 0.05) < 1e-9);
evalin('base', 'clear(''nfrk_y'')');
%=============================================================================
fr = [tempname(), '.nflow'];
[~, frstem] = fileparts(fr);
fid = fopen(fr, 'wt');
fwrite(fid, jsonencode(dr));
fclose(fid);
nflow_codegenerate(fr, tempdir(), false, 'C');
srcR = fileread([tempdir(), frstem, '/model.c']);
assert_istrue(isempty(strfind(srcR, 'unknown:')));
assert_istrue(~isempty(strfind(srcR, '__nflow_rhs'))); % on the RK4 path
% The packed state is read TWICE: once before the reporting evaluation, once
% after it, which is where a reset re-seeds the member's field.
assert_isequal(numel(strfind(srcR, '__x0[0] = s->int_sub_ig;')), 2);
% The control latch runs on the reporting pass only.
assert_istrue(~isempty(strfind(srcR, '  if (out) {')));
rmfile(fr);
%=============================================================================
if __nflow_codegen_toolchain__('C')
  % The numeric check builds the runner and compares its CSV with the
  % simulation sample by sample; the reset sample is what used to differ.
  nflow_codegen_check_numeric(dr, 'nfrk_num', 'C', {'y'}, {'nfrk_y'}, 5e-6, 1e-9);
  % And that runner walks k*dt rather than accumulating dt into itself.
  mainR = fileread([tempdir(), 'nfrk_num/main.c']);
  assert_istrue(~isempty(strfind(mainR, 'const double t = __k * dt;')));
  assert_istrue(isempty(strfind(mainR, 't += dt')));
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dr, 'nfrk_num', 'rust', {'y'}, {'nfrk_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
% A variable-step solver keeps the refusal: it can reject a step and retry it,
% which would replay the re-seed, so the reset gate does not lower there.
dr45 = dr;
dr45.solver = 'ode45';
f45 = [tempname(), '.nflow'];
fid = fopen(f45, 'wt');
fwrite(fid, jsonencode(dr45));
fclose(fid);
msg45 = '';
try
  nflow_codegenerate(f45, tempdir(), false, 'C');
catch e45
  msg45 = e45.message;
end
rmfile(f45);
assert_istrue(~isempty(strfind(lower(msg45), 'conditional')));
%=============================================================================
