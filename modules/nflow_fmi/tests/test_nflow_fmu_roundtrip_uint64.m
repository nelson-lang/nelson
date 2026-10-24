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
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');

% UInt64 boundary signal: constant(7) -> convert(uint64) -> external output. The
% FMU exposes a UInt64 variable; the round-trip reads it via fmi3GetUInt64. (The
% signal path is double internally, so values above 2^53 are not exact -- a
% documented limitation; the boundary type itself is exact.)
du = struct();
du.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 7)), ...
  struct('id', 'c', 'type', 'convert', 'inputs', 1, 'outputs', 1, ...
    'params', struct('OutDataType', 'uint64')), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
du.connections = {...
  struct('from', 'k', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
du.sampleTime = 0.1; du.duration = 0.5; du.variables = struct();
modelFileU = [tempname(), '.nflow'];
fid = fopen(modelFileU, 'wt'); fwrite(fid, jsonencode(du)); fclose(fid);
fmuU = NFlow.exportfmu(modelFileU, tempdir());
coU = tempname();
if isdir(coU)
  rmdir(coU, 's');
end
namesU = unzip(fmuU, coU);
mdU = fileread([coU, filesep, 'modelDescription.xml']);
assert_istrue(~isempty(strfind(mdU, '<UInt64 name="y"')));
hasBinaryU = any(cellfun(@(s) endsWith(s, getdynlibext()), namesU));
if hasBinaryU
  d4 = struct();
  d4.blocks = {...
    struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuU)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'rt_yu', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d4.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d4.sampleTime = 0.1; d4.duration = 0.5; d4.variables = struct();
  ru = jsondecode(__nflow_simulate__(jsonencode(d4)));
  assert_isequal(ru.status, 'done');
  yu = evalin('base', 'rt_yu'); yu = yu(:);
  assert_istrue(max(abs(yu - 7.0)) < 1e-9);
  evalin('base', 'clear(''rt_yu'')');
end
%=============================================================================
