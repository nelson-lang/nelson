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
% Numeric equivalence of GENERATED code vs the interpreter, compiled and
% executed through the local toolchain (cmake / cargo): the fused-Euler path,
% for BOTH targets.
%=============================================================================
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
%=============================================================================
% Model A -- fused Euler: x' = 1 through an integrator, y(t) = t.
d = struct();
d.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfcg_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'k', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
nflow_codegen_check_numeric(d, 'nfcg_euler', 'C', {'y'}, {'nfcg_y'}, 5e-6, 1e-9);
if __nflow_codegen_toolchain__('rust')
  nflow_codegen_check_numeric(d, 'nfcg_euler', 'rust', {'y'}, {'nfcg_y'}, 5e-6, 1e-9);
end
%=============================================================================
