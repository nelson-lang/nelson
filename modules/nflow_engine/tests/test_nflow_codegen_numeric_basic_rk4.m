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
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
%=============================================================================
% Model B -- packed-state RK4 (ode4): first-order lag x' = 1 - x, y = x.
d2 = struct();
d2.blocks = {...
  struct('id', 'k1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '+-')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfcg_lag', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {...
  struct('from', 'k1', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.05;
d2.duration = 1.0;
d2.solver = 'ode4';
d2.variables = struct();
[~, uniqueTag] = fileparts(tempname());
stem = ['nfcg_rk4_', uniqueTag];
nflow_codegen_check_numeric(d2, stem, 'C', {'y'}, {'nfcg_lag'}, 5e-6, 1e-9);
% The generated model must actually be on the RK4 path (not the Euler
% fallback): its source carries the shared rhs.
src = fileread([tempdir(), stem, '/model.c']);
assert_istrue(~isempty(strfind(src, '__nflow_rhs')));
if __nflow_codegen_toolchain__('rust')
  nflow_codegen_check_numeric(d2, stem, 'rust', {'y'}, {'nfcg_lag'}, 5e-6, 1e-9);
end
if isdir([tempdir(), stem])
  rmdir([tempdir(), stem], 's');
end
%=============================================================================
