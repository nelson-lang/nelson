%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% The fileSource block carries its (times, values) table in its params, so it is
% code-generatable: the generators emit a static table plus the same rewind-safe
% piecewise-constant lookup the simulator uses (previously the codegen emitted a
% silent 0.0 placeholder). The compiled FMU must reproduce the direct simulation
% exactly.
%
% times = [0, 0.5, 1.0], values = [10, 20, 30], base step 0.25 ->
% staircase 10,10,20,20,30,30,... (largest index with times[i] <= t).
%=============================================================================
% Rust backend structural check (no toolchain needed): fileSource lowers to the
% same table + lookup, not the old 0.0/TODO placeholder. The compiled Rust
% reproduces the staircase (verified with cargo build+run during development).
dR = struct();
dR.blocks = {...
  struct('id', 'fs', 'type', 'fileSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('times', [0, 0.5, 1.0], 'values', [10, 20, 30])), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dR.connections = {struct('from', 'fs', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
dR.sampleTime = 0.25; dR.duration = 1.25; dR.variables = struct();
dR_dir = tempname(); mkdir(dR_dir);
cR = onCleanup(@() rmdir(dR_dir, 's')); %#ok<NASGU>
mfR = fullfile(dR_dir, 'fs_rust.nflow');
fid = fopen(mfR, 'wt'); fwrite(fid, jsonencode(dR)); fclose(fid);
outR = tempname();
if isdir(outR)
  rmdir(outR, 's');
end
mkdir(outR);
nflow_codegenerate(mfR, outR, false, 'rust');
libR = fileread([outR, filesep, 'fs_rust', filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(libR, '__fsrc_v_'))); % emitted the table
assert_istrue(isempty(strfind(libR, 'TODO'))); % not the placeholder
rmfile(mfR);
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
tv = struct('times', [0, 0.5, 1.0], 'values', [10, 20, 30]);
%=============================================================================
% Direct simulation.
dsim = struct();
dsim.blocks = {...
  struct('id', 'fs', 'type', 'fileSource', 'inputs', 0, 'outputs', 1, 'params', tv), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'fsrc_ys', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dsim.connections = {struct('from', 'fs', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dsim.sampleTime = 0.25; dsim.duration = 1.25; dsim.runtime = 1.25; dsim.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(dsim)));
assert_isequal(r.status, 'done');
ys = evalin('base', 'fsrc_ys'); ys = ys(:)';
evalin('base', 'clear(''fsrc_ys'')');
% The expected staircase.
assert_isequal(ys(1:6), [10, 10, 20, 20, 30, 30]);
%=============================================================================
% Export to an FMU (generated C carries the table + lookup) and co-simulate.
dexp = struct();
dexp.blocks = {...
  struct('id', 'fs', 'type', 'fileSource', 'inputs', 0, 'outputs', 1, 'params', tv), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dexp.connections = {struct('from', 'fs', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
dexp.sampleTime = 0.25; dexp.duration = 1.25; dexp.variables = struct();
modelFile = [tempname(), '.nflow'];
fid = fopen(modelFile, 'wt'); fwrite(fid, jsonencode(dexp)); fclose(fid);
fmu = NFlow.exportfmu(modelFile, tempdir());

co = tempname();
if isdir(co)
  rmdir(co, 's');
end
names = unzip(fmu, co);
hasBinary = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBinary, 'no C toolchain: FMU exported source-only, cannot load');

d2 = struct();
d2.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'fsrc_yf', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.25; d2.duration = 1.25; d2.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r.status, 'done');
yf = evalin('base', 'fsrc_yf'); yf = yf(:)';
evalin('base', 'clear(''fsrc_yf'')');
% The compiled table lookup reproduces the simulation exactly.
n = min(numel(ys), numel(yf));
assert_istrue(max(abs(ys(1:n) - yf(1:n))) < 1e-12);
rmfile(modelFile);
%=============================================================================
