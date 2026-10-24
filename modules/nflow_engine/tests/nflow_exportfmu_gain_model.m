%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = nflow_exportfmu_gain_model()
  d = struct();
  d.blocks = {...
    struct('id', 'in', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
    struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true))};
  d.connections = {...
    struct('from', 'in', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.01;
  d.duration = 5.0;
  d.variables = struct();
end
