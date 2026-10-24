%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ENGLISH IMPOSED-->
%=============================================================================
dc = struct();
% A function-call generator firing THREE times per step: those calls are a loop
% within the step, carrying state from one to the next, which straight-line code
% cannot express. (One call per step lowers now, as does the action-selector
% family, so neither is the probe any more.)
dc.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'sel', 'type', 'functionCallGenerator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('NumberOfIterations', 3))};
dc.connections = {struct('from', 'clk', 'to', 'sel', 'fromIndex', 0, 'toIndex', 0)};
wd = tempname();
mkdir(wd);
cleaner = onCleanup(@() rmdir(wd, 's')); %#ok<NASGU>
condFile = fullfile(wd, 'fmu_export_cond.nflow');
fid = fopen(condFile, 'wt'); fwrite(fid, jsonencode(dc)); fclose(fid);
msg = '';
try
  NFlow.exportfmu(condFile, wd);
catch e
  msg = e.message;
end
asserts.istrue(~isempty(strfind(lower(msg), 'conditional')));
%=============================================================================
