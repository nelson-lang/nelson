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
% Opt-in iterative co-simulation coupling. The engine iterates the
% FMU doSteps with state rollback until the coupling outputs converge. This test
% validates the accuracy gain on a genuinely coupled FMU feedback loop: two
% exported lag FMUs wired head-to-tail are compared against the analytic
% closed-loop solution; the converged Gauss-Seidel iteration lands strictly
% closer than the single Jacobi pass.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
% Accuracy gain on a genuinely coupled loop. Two exported lag FMUs:
%   A: x1' = -x1 + u1 + 1 (internal forcing), y1 = x1
%   B: x2' = -x2 + u2,                        y2 = x2
% coupled u1 = y2, u2 = y1. Analytic: x1(t) = (t + (1 - exp(-2 t))/2) / 2.
% Each FMU sub-steps internally at 0.01 (its intra-FMU error is negligible);
% the macro loop runs at 0.1, so the COUPLING error dominates. The converged
% Gauss-Seidel iteration must land strictly closer to the analytic solution
% than the default single Jacobi pass.
function d = coupling_lag_model(withForcing)
  if withForcing
    blocks = {...
      struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
        'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
      struct('id', 'k1', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
      struct('id', 's', 'type', 'sum', 'inputs', 3, 'outputs', 1, 'params', struct('Inputs', '++-')), ...
      struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
      struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
        'params', struct('GotoTag', 'y', 'isExternalPort', true))};
    conns = {...
      struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'k1', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
      struct('from', 'ig', 'to', 's', 'fromIndex', 0, 'toIndex', 2), ...
      struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
  else
    blocks = {...
      struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
        'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
      struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, 'params', struct('Inputs', '+-')), ...
      struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
      struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
        'params', struct('GotoTag', 'y', 'isExternalPort', true))};
    conns = {...
      struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'ig', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
      struct('from', 's', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
      struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
  end
  d = struct();
  d.blocks = blocks;
  d.connections = conns;
  d.sampleTime = 0.01; d.duration = 2.0; d.variables = struct();
end
%=============================================================================
fA = [tempname(), '.nflow'];
fB = [tempname(), '.nflow'];
dA = coupling_lag_model(true);
dB = coupling_lag_model(false);
fid = fopen(fA, 'wt'); fwrite(fid, jsonencode(dA)); fclose(fid);
fid = fopen(fB, 'wt'); fwrite(fid, jsonencode(dB)); fclose(fid);
fmuA = NFlow.exportfmu(fA, tempdir());
fmuB = NFlow.exportfmu(fB, tempdir());
coA = tempname();
if isdir(coA); rmdir(coA, 's'); end
namesA = unzip(fmuA, coA);
hasBinaryA = any(cellfun(@(s) endsWith(s, getdynlibext()), namesA));
if hasBinaryA
  dc = struct();
  dc.blocks = {...
    struct('id', 'fa', 'type', 'fmu', 'inputs', 1, 'outputs', 1, 'params', struct('path', fmuA)), ...
    struct('id', 'fb', 'type', 'fmu', 'inputs', 1, 'outputs', 1, 'params', struct('path', fmuB)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'cpl_y1', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  dc.connections = {...
    struct('from', 'fa', 'to', 'fb', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'fb', 'to', 'fa', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'fa', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  dc.sampleTime = 0.1; dc.duration = 2.0; dc.solver = 'discrete'; dc.variables = struct();

  rs = jsondecode(__nflow_simulate__(jsonencode(dc)));
  assert_isequal(rs.status, 'done');
  y1 = evalin('base', 'cpl_y1'); y1 = y1(:); evalin('base', 'clear(''cpl_y1'')');

  dc.iterativeCoupling = true; dc.couplingMaxIter = 30; dc.couplingTol = 1e-12;
  ri = jsondecode(__nflow_simulate__(jsonencode(dc)));
  assert_isequal(ri.status, 'done');
  y2 = evalin('base', 'cpl_y1'); y2 = y2(:); evalin('base', 'clear(''cpl_y1'')');

  n = min(numel(y1), numel(y2));
  t = (0:n - 1)' * 0.1;
  xa = (t + (1 - exp(-2 * t)) / 2) / 2;
  errSingle = max(abs(y1(1:n) - xa));
  errIter = max(abs(y2(1:n) - xa));
  % Closed loop is tracked at all (the export bug drove the loop open: ~2.0).
  assert_istrue(errSingle < 0.1);
  assert_istrue(errIter < 0.1);
  % The converged iteration lands strictly closer than the single pass.
  assert_istrue(errIter < errSingle);
end
%=============================================================================
