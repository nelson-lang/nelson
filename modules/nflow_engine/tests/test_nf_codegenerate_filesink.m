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
% fileSink gained both halves. Simulation: a streaming CSV writer (INIT
% truncates and writes the header, AFTER_STEP appends one row per sample).
% Code generation: the pipeline rewrites the block into an external
% labelSink, so the value becomes a model-output column of the generated
% runner's CSV (tagged with the block id) with no file I/O in model.c.
%=============================================================================
csvf = [tempname(), '.csv'];
d = struct();
d.blocks = {...
  struct('id', 's', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 2, 'Frequency', 1)), ...
  struct('id', 'fs', 'type', 'fileSink', 'inputs', 1, 'outputs', 0, ...
    'params', struct('FileName', csvf)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nffs_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 's', 'to', 'fs', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.05; % duration >= 10*dt keeps the engine from padding the run
d.duration = 0.5;
d.variables = struct();
%=============================================================================
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nffs_y');
assert_istrue(isfile(csvf));
txt = fileread(csvf);
lines = strsplit(strtrim(txt), sprintf('\n'));
assert_isequal(strtrim(lines{1}), 't,fs');
assert_isequal(numel(lines), 12); % header + 11 samples (t = 0:0.05:0.5)
row2 = strsplit(strtrim(lines{2}), ',');
assert_isequal(str2double(row2{1}), 0);
assert_istrue(abs(str2double(row2{2}) - ysim(1)) < 1e-12);
rowEnd = strsplit(strtrim(lines{end}), ',');
assert_istrue(abs(str2double(rowEnd{1}) - 0.5) < 1e-12);
assert_istrue(abs(str2double(rowEnd{2}) - ysim(end)) < 1e-12);
rmfile(csvf);
evalin('base', 'clear(''nffs_y'')');
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(isempty(strfind(src, 'unknown:')));
assert_istrue(isempty(strfind(src, 'fopen'))); % no file I/O in generated code
assert_istrue(~isempty(strfind(src, 'out->fs = out_fs;'))); % model-output field
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(isempty(strfind(srcR, 'unknown:')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets): the fileSink
% column of the generated runner tracks the simulation samples.
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nffs_num', 'C', {'fs'}, {'nffs_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nffs_num', 'rust', {'fs'}, {'nffs_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
