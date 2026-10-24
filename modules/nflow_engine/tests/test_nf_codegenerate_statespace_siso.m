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
% SISO regression: the historical SISO Euler form advanced the state FIRST
% and read the CURRENT input (a whole-sample lead over the simulation, plus a
% live D term the runtime never applies). Every form now reports at the
% pre-step state with the latched previous-step input. A time-varying input
% with D ~= 0 pins it: this diverged by ~0.7 absolute before the fix.
ds = struct();
ds.blocks = {...
  struct('id', 's', 'type', 'sine', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Amplitude', 2, 'Frequency', 1)), ...
  struct('id', 'ss', 'type', 'stateSpace', 'inputs', 1, 'outputs', 1, ...
    'params', struct('A', -1, 'B', 1, 'C', 1, 'D', 0.5)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfsi_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
ds.connections = {...
  struct('from', 's', 'to', 'ss', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ss', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ss', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
ds.sampleTime = 0.1;
ds.duration = 1.0;
ds.variables = struct();
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(ds, 'nfsi_num', 'C', {'y'}, {'nfsi_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(ds, 'nfsi_num', 'rust', {'y'}, {'nfsi_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
