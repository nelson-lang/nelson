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
% rateTransition used to hit the unknownTypes error. Its codegen is the zoh
% shape: a held sample with time-based self-scheduling on OutPortSampleTime
% (output-then-update, like the simulator's OUTPUT/UPDATE split); <= base
% step or the default -1 samples every step.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'rt', 'type', 'rateTransition', 'inputs', 1, 'outputs', 1, ...
    'params', struct('OutPortSampleTime', 0.3, 'Initial', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrt_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'rt', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rt', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rt', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'rt_next_rt')));
assert_istrue(isempty(strfind(src, 'unknown:')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'rt_next_rt')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets).
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfrt_num', 'C', {'y'}, {'nfrt_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfrt_num', 'rust', {'y'}, {'nfrt_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
