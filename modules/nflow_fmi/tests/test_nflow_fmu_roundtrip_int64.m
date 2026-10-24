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

% Int64 boundary signal: constant(5) -> convert(int64) -> external output.
% The FMU exposes an Int64 variable; the round-trip reads it via fmi3GetInt64.
di = struct();
di.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'c', 'type', 'convert', 'inputs', 1, 'outputs', 1, ...
    'params', struct('OutDataType', 'int64')), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
di.connections = {...
  struct('from', 'k', 'to', 'c', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'c', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
di.sampleTime = 0.1; di.duration = 0.5; di.variables = struct();
modelFileI = [tempname(), '.nflow'];
fid = fopen(modelFileI, 'wt'); fwrite(fid, jsonencode(di)); fclose(fid);
fmuI = NFlow.exportfmu(modelFileI, tempdir());

coI = tempname();
if isdir(coI)
  rmdir(coI, 's');
end
namesI = unzip(fmuI, coI);
mdI = fileread([coI, filesep, 'modelDescription.xml']);
assert_istrue(~isempty(strfind(mdI, '<Int64 name="y"')));
hasBinaryI = any(cellfun(@(s) endsWith(s, getdynlibext()), namesI));
if hasBinaryI
  d3 = struct();
  d3.blocks = {...
    struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuI)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'rt_yi', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d3.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d3.sampleTime = 0.1; d3.duration = 0.5; d3.variables = struct();
  ri = jsondecode(__nflow_simulate__(jsonencode(d3)));
  assert_isequal(ri.status, 'done');
  yi = evalin('base', 'rt_yi'); yi = yi(:);
  assert_istrue(max(abs(yi - 5.0)) < 1e-9);
  evalin('base', 'clear(''rt_yi'')');
end
%=============================================================================
