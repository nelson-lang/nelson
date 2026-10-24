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
%=============================================================================
% A vector fromWorkspace (width > 1, values baked column-major as
% values[k*width + i] over a shared times vector) code-generates: the vector-
% expand pass slices it into one scalar fromWorkspace per channel, each with its
% own column of data, and the scalar generator emits a per-channel static table +
% lookup. This must NOT hit the "no baked data" 0.0 fallback (which is reserved
% for a genuinely unbaked live-workspace-variable reference).
%=============================================================================
% width 2: channel0=[10,20,30], channel1=[1,2,3] -> flat [10,1,20,2,30,3].
tv = struct('times', [0, 0.5, 1.0], 'values', [10, 1, 20, 2, 30, 3], 'width', 2);
d = struct();
d.blocks = {...
  struct('id', 'fw', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, 'params', tv), ...
  struct('id', 'o', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {struct('from', 'fw', 'to', 'o', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.25; d.duration = 0.75; d.variables = struct();
model = [tempname(), '.nflow'];
[~, modelstem] = fileparts(model);
fid = fopen(model, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
%=============================================================================
outDir = tempname();
if isdir(outDir)
  rmdir(outDir, 's');
end
mkdir(outDir);
nflow_codegenerate(model, outDir, false, 'C');
txt = fileread([outDir, filesep, modelstem, filesep, 'model.c']);
% The vector-expand pass sliced fromWorkspace into per-channel blocks (fw__e...)
% and the scalar generator emitted the shared workspace lookup helper -- i.e. it
% lowered the baked table, NOT the silent no-baked-data 0.0 fallback. (An unused
% channel may be dead-code eliminated, so we assert on the mechanism, not on a
% specific channel's values.)
assert_istrue(~isempty(strfind(txt, 'fw__e')));
assert_istrue(~isempty(strfind(txt, 'nflow_from_workspace')));
assert_istrue(isempty(strfind(txt, 'no baked data')));
%=============================================================================
% Rust lowers it the same way (structural, no toolchain).
outDirR = tempname();
if isdir(outDirR)
  rmdir(outDirR, 's');
end
mkdir(outDirR);
nflow_codegenerate(model, outDirR, false, 'rust');
libR = fileread([outDirR, filesep, modelstem, filesep, 'src', filesep, 'lib.rs']);
assert_istrue(~isempty(strfind(libR, 'fw__e')));
assert_istrue(~isempty(strfind(libR, 'nflow_from_workspace')));
assert_istrue(isempty(strfind(libR, 'no baked data')));
%=============================================================================
rmfile(model);
%=============================================================================
