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
% Conditional execution V1: an enable-gated subsystem now lowers. The
% flattener captures the control wire into a synthetic condGate, tags every
% member with the gate id (the generators wrap their emission in the guard,
% so a closed gate skips outputs AND state updates), and routes each external
% output through a condHold that reproduces the runtime's held outputs.
% Model: sine -> [enable: gain 2 -> unitDelay] -> out, control = (t < 0.55).
% While disabled the output holds and the unitDelay state freezes.
%=============================================================================
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2)), ...
  struct('id', 'ud', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'ud', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ud', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
d = struct();
d.blocks = {...
  struct('id', 'src', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 2, 'Frequency', 1)), ...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'ctl', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, ...
    'params', struct('relop', 'lt', 'const', 0.55)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', inner, 'controlPort', struct('kind', 'enable'))), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfce_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'src', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'clk', 'to', 'ctl', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ctl', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
%=============================================================================
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfce_y');
% Disabled from t = 0.6 on: the output holds its last enabled value.
assert_istrue(abs(ysim(8) - ysim(7)) < 1e-12);
assert_istrue(abs(ysim(end) - ysim(7)) < 1e-12);
assert_istrue(abs(ysim(7)) > 1e-3); % and that value is not trivially zero
evalin('base', 'clear(''nfce_y'')');
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(isempty(strfind(src, 'unknown:')));
% The guard wraps the members and the hold replays the boundary.
assert_istrue(~isempty(strfind(src, 'if (out_sub__nfg != 0.0) {')));
assert_istrue(~isempty(strfind(src, 'ch_sub__nfh0')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(isempty(strfind(srcR, 'unknown:')));
assert_istrue(~isempty(strfind(srcR, 'if out_sub__nfg != 0.0 {')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets): hold, state
% freeze and re-read all match sample for sample.
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfce_num', 'C', {'y'}, {'nfce_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfce_num', 'rust', {'y'}, {'nfce_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
