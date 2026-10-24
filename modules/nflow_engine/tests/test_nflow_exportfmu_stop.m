%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--ENGLISH IMPOSED-->
%=============================================================================
ds = nflow_exportfmu_gain_model();
ds.blocks{end + 1} = struct('id', 'cmp', 'type', 'compareToConstant', ...
  'inputs', 1, 'outputs', 1, 'params', struct('relop', 'ge', 'const', 1.0));
ds.blocks{end + 1} = struct('id', 'stop', 'type', 'stopSimulation', ...
  'inputs', 1, 'outputs', 0, 'params', struct());
ds.connections{end + 1} = struct('from', 'g', 'to', 'cmp', 'fromIndex', 0, 'toIndex', 0);
ds.connections{end + 1} = struct('from', 'cmp', 'to', 'stop', 'fromIndex', 0, 'toIndex', 0);
wd = tempname();
mkdir(wd);
cleaner = onCleanup(@() rmdir(wd, 's')); %#ok<NASGU>
stopFile = fullfile(wd, 'fmu_export_stop.nflow');
fid = fopen(stopFile, 'wt'); fwrite(fid, jsonencode(ds)); fclose(fid);
fmuS = NFlow.exportfmu(stopFile, wd);
coS = fullfile(wd, 'fmu_export_stop_check');
unzip(fmuS, coS);
wrapS = fileread([coS, filesep, 'sources', filesep, 'fmi3Functions.c']);
asserts.istrue(~isempty(strfind(wrapS, 'nflow_stop_requested != 0.0) break;')));
asserts.istrue(~isempty(strfind(wrapS, ...
  '*terminateSimulation = (c->state.nflow_stop_requested != 0.0) ? fmi3True : fmi3False;')));
%=============================================================================
