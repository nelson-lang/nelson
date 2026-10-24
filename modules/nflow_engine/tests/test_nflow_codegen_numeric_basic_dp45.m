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
% Model C -- pure adaptive DP45 (ode45), no crossing surfaces: the same lag
% under the embedded Dormand-Prince stepper. The crossing variant is covered
% by test_nf_codegenerate_zerocross.m; until now the smooth adaptive loop of
% the standalone runner was never executed against the simulator.
d3 = struct();
d3.blocks = {...
  struct('id', 'k1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '+-')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfcg_lag45', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d3.connections = {...
  struct('from', 'k1', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.05;
d3.duration = 1.0;
d3.solver = 'ode45';
d3.variables = struct();
[~, uniqueTag] = fileparts(tempname());
stem = ['nfcg_dp45_', uniqueTag];
nflow_codegen_check_numeric(d3, stem, 'C', {'y'}, {'nfcg_lag45'}, 5e-6, 1e-6);
% Seven stages + the error controller, and none of the root-localization
% machinery: this must be the plain DP45 stepper, not the -WithRoots one.
src45 = fileread([tempdir(), stem, '/model.c']);
assert_istrue(~isempty(strfind(src45, '__k7')));
assert_istrue(~isempty(strfind(src45, '__rtol')));
assert_istrue(isempty(strfind(src45, '__nflow_rk4sub')));
if __nflow_codegen_toolchain__('rust')
  nflow_codegen_check_numeric(d3, stem, 'rust', {'y'}, {'nfcg_lag45'}, 5e-6, 1e-6);
end
if isdir([tempdir(), stem])
  rmdir([tempdir(), stem], 's');
end
%=============================================================================
