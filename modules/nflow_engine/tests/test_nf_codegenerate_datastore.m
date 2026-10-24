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
% dataStoreMemory / Write / Read used to hit the unknownTypes error. The
% named store becomes ONE shared ModelState field per DataStoreName; a Read
% has no wired input so the topological sort emits it before any Write,
% reproducing the simulator's one-step latency (Read in OUTPUT sees the
% previous step's Write in UPDATE). Memory's InitialValue init is appended
% after the shared 0.0 seed, so it wins whatever the block order.
% Model: Memory(X, 5); clock -> Write(X); Read(X) -> y.
% y = the clock itself: every write of a store runs before every read of it, so
% the read answers with what this step wrote. The Memory block's InitialValue
% only shows while nothing has written yet, which here is never.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'mem', 'type', 'dataStoreMemory', 'inputs', 0, 'outputs', 0, ...
    'params', struct('DataStoreName', 'X', 'InitialValue', 5)), ...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'wr', 'type', 'dataStoreWrite', 'inputs', 1, 'outputs', 0, ...
    'params', struct('DataStoreName', 'X')), ...
  struct('id', 'rd', 'type', 'dataStoreRead', 'inputs', 0, 'outputs', 1, ...
    'params', struct('DataStoreName', 'X')), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfds_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'wr', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rd', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rd', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.5;
d.variables = struct();
%=============================================================================
% Simulator reference: the clock, read in the step that wrote it.
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfds_y');
assert_istrue(abs(ysim(1)) < 1e-12);
assert_istrue(abs(ysim(3) - 0.2) < 1e-12);
evalin('base', 'clear(''nfds_y'')');
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'double ds_X;')));
assert_istrue(~isempty(strfind(src, 's->ds_X = 5;')));
assert_istrue(isempty(strfind(src, 'unknown:')));
% The Write's emission precedes the Read's: the read answers with the value
% this step wrote, and the two share no wire to say so on their own.
assert_istrue(min(strfind(src, 's->ds_X = out_wr;')) < min(strfind(src, 'out_rd = s->ds_X;')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'pub ds_X: f64,')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets).
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfds_num', 'C', {'y'}, {'nfds_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfds_num', 'rust', {'y'}, {'nfds_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
