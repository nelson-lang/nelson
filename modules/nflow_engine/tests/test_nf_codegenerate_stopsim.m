%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% stopSimulation used to hit the unknownTypes error. Generated code now
% mirrors the simulator: a shared nflow_stop_requested state flag raised at
% the block's position, and the generated main() ends its loop AFTER writing
% the stopping step's row (the simulator stops after that step too).
% Model: clock >= 0.25 fires at t = 0.3 with dt = 0.1 -> rows t = 0..0.3.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'cmp', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, ...
    'params', struct('relop', 'ge', 'const', 0.25)), ...
  struct('id', 'st', 'type', 'stopSimulation', 'inputs', 1, 'outputs', 0, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'clk', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cmp', 'to', 'st', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'clk', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
%=============================================================================
nflow_codegenerate(f, tempdir(), true, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'double nflow_stop_requested;')));
assert_istrue(~isempty(strfind(src, 's->nflow_stop_requested = 1.0;')));
assert_istrue(isempty(strfind(src, 'unknown:')));
mainSrc = fileread([tempdir(), fstem, '/main.c']);
assert_istrue(~isempty(strfind(mainSrc, 'if (state.nflow_stop_requested != 0.0) break;')));
%=============================================================================
nflow_codegenerate(f, tempdir(), true, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'pub nflow_stop_requested: f64,')));
assert_istrue(~isempty(strfind(srcR, 's.nflow_stop_requested = 1.0;')));
mainR = fileread([tempdir(), fstem, '/src/main.rs']);
assert_istrue(~isempty(strfind(mainR, 'if state.nflow_stop_requested != 0.0 { break; }')));
%=============================================================================
% Compiled proof: the emitted binary stops after the t = 0.3 row (4 rows).
if __nflow_codegen_toolchain__('C')
  nflow_codegenerate(f, tempdir(), true, 'C', struct('build', true, 'buildConfig', 'Release'));
  if ispc()
    exe = [tempdir(), fstem, '/build/Release/nflow_run.exe'];
  else
    exe = [tempdir(), fstem, '/build/nflow_run'];
  end
  csv = [tempname(), '.csv'];
  [st, out] = system(['"', exe, '" -t 1.0 -o "', csv, '"']);
  assert_isequal(st, 0, out);
  txt = fileread(csv);
  rows = strsplit(strtrim(txt), sprintf('\n'));
  assert_isequal(numel(rows), 1 + 4); % header + t = 0, 0.1, 0.2, 0.3
end
%=============================================================================
rmfile(f);
%=============================================================================
