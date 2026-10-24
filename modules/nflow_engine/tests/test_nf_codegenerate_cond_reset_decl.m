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
% Reset must re-seed EVERY member state, including states declared through
% the declarative `state` vector (derivative's der_prev) and raw
% declState/addInit lines (ddelay's circular buffer); both used to be
% skipped (only sa.addState members were collected), so a ddelay or
% derivative inside a reset subsystem silently kept its pre-reset state in
% generated code.
%=============================================================================
dr = nflow_cond_enable_base_diagram('nfcr_y');
dr.blocks{4}.params.controlPort = struct('kind', 'reset', 'resetTriggerType', 'rising');
dr.blocks{4}.params.subsystem.controlPort = struct('kind', 'reset');
dr.blocks{3}.params = struct('relop', 'ge', 'const', 0.35);
innerR = struct();
innerR.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'der', 'type', 'derivative', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dd', 'type', 'ddelay', 'inputs', 1, 'outputs', 1, ...
    'params', struct('DelayLength', 3, 'SampleTime', 0.1)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
innerR.connections = {...
  struct('from', 'in1', 'to', 'der', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'der', 'to', 'dd', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dd', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
innerR.externalInputs = {struct('id', 'in1', 'name', 'in1')};
innerR.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
innerR.controlPort = struct('kind', 'reset');
dr2 = dr;
dr2.blocks{4}.params.subsystem = innerR;
dr2.blocks{6}.params.VariableName = 'nfcs_y';
r3b = jsondecode(__nflow_simulate__(jsonencode(dr2)));
assert_isequal(r3b.status, 'done');
evalin('base', 'clear(''nfcs_y'')');
fr2 = [tempname(), '.nflow'];
[~, fr2stem] = fileparts(fr2);
fid = fopen(fr2, 'wt');
fwrite(fid, jsonencode(dr2));
fclose(fid);
nflow_codegenerate(fr2, tempdir(), false, 'C');
srcR2 = fileread([tempdir(), fr2stem, '/model.c']);
% The re-seed block replays BOTH forms: derivative's der_prev field and
% ddelay's raw buffer loop. The replayed addInit line carries the re-seed
% block's extra indent, discriminating it from the nflow_init copy.
assert_istrue(~isempty(strfind(srcR2, '    s->der_prev_')));
assert_isequal(numel(strfind(srcR2, '      for (int i = 0; i < 3; i++) s->ddelay_buf_')), 1);
rmfile(fr2);
% Compiled-numeric parity of the derivative -> ddelay chain inside a reset
% subsystem. This used to be a KNOWN divergence: the derivative was emitted
% un-latched, so a derivative (or a chain feeding a ddelay) inside a
% subsystem read one sample ahead of the simulator. Since the latched-
% derivative fix the flattened chain matches sample for sample, reset edge
% included -- both targets. A 2 s window makes the delayed, reset-affected
% signal observable (the delay-3 buffer keeps the first samples at zero).
dr2n = dr2;
dr2n.duration = 2.0;
dr2n.blocks{6}.params.VariableName = 'nfcs_num_y';
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dr2n, 'nfcs_num', 'C', {'y'}, {'nfcs_num_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dr2n, 'nfcs_num', 'rust', {'y'}, {'nfcs_num_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
