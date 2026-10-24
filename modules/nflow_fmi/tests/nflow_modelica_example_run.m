%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [t, y] = nflow_modelica_example_run(moFile, modelName, stopTime, dt, outVar)
  m = struct('id', 'phys', 'type', 'modelica', 'inputs', 0, 'outputs', 1, ...
    'params', struct('file', moFile, 'modelName', modelName));
  tw = struct('id', 'out', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', outVar));
  conn = struct('from', 'phys', 'to', 'out', 'fromIndex', 0, 'toIndex', 0);
  doc = struct('blocks', {{m, tw}}, 'connections', {{conn}}, ...
    'runtime', stopTime, 'sampleTime', dt);
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w');
  fwrite(fid, jsonencode(doc));
  fclose(fid);
  out = sim(p);
  t = out.tout;
  y = out.logsout(1).values;
end
%=============================================================================
