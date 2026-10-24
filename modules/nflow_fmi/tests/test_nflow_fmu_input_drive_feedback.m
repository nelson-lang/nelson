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
% <--ENGLISH IMPOSED-->
%=============================================================================
% FMU export: a lag with feedback (sum -> integrator -> back into the sum) must
% pre-load the integrator's output before the sum reads it. Without the pre-load
% the feedback term reads 0.0 and the FMU integrates the open loop.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
% Lag with internal forcing: x' = 1 - x, y = x -> y(t) = 1 - exp(-t).
% The model's fine internal step (0.01) keeps the Euler error ~5e-3; the old
% broken emission produced y = t (error ~0.14 at t=1), far outside tolerance.
dl = struct();
dl.blocks = {...
  struct('id', 'k1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '+-')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dl.connections = {...
  struct('from', 'k1', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
dl.sampleTime = 0.01; dl.duration = 1.0; dl.variables = struct();
mfl = [tempname(), '.nflow'];
fid = fopen(mfl, 'wt'); fwrite(fid, jsonencode(dl)); fclose(fid);
fmuL = NFlow.exportfmu(mfl, tempdir());
rl = fmiCoSimulate(fmuL, 1.0, 0.1);
ya = 1 - exp(-rl.time);
assert_istrue(max(abs(rl.outputs - ya)) < 1e-2);
%=============================================================================
