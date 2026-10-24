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
%=============================================================================
% Pass-through subsystem (extInput wired straight to extOutput): both port
% blocks are removed by the flattener, which used to drop the wire entirely
% and emit a reference to a nonexistent producer. The outer source must now
% reach the outer consumer directly.
innerP = struct();
innerP.blocks = {...
  struct('id', 'pin', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'pin')), ...
  struct('id', 'pout', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'pout'))};
innerP.connections = {struct('from', 'pin', 'to', 'pout', 'fromIndex', 0, 'toIndex', 0)};
innerP.externalInputs = {struct('id', 'pin', 'name', 'pin')};
innerP.externalOutputs = {struct('id', 'pout', 'name', 'pout')};
dp = struct();
dp.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 1, 'outputs', 1, ...
    'params', struct('subsystem', innerP)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfpt_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
dp.connections = {...
  struct('from', 'c', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
dp.sampleTime = 0.1;
dp.duration = 1.0;
dp.variables = struct();
fp = [tempname(), '.nflow'];
[~, fpstem] = fileparts(fp);
fid = fopen(fp, 'wt');
fwrite(fid, jsonencode(dp));
fclose(fid);
nflow_codegenerate(fp, tempdir(), false, 'C');
srcP = fileread([tempdir(), fpstem, '/model.c']);
assert_istrue(~isempty(strfind(srcP, 'out_y = out_c;'))); % wire survives
rmfile(fp);
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dp, 'nfpt_num', 'C', {'y'}, {'nfpt_y'}, 0, 0);
end
%=============================================================================
