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
%=============================================================================
% A1.1 (Newton fallback): a non-contractive / ill-conditioned algebraic loop
% that Gauss-Seidel provably diverges on is solved by the Newton fallback.
%
%   s = u + K*s   =>   s = u / (1 - K).
%
% For K > 1 the fixed-point sweep s_{n+1} = u + K*s_n diverges (|K| > 1), so the
% legacy Gauss-Seidel iterator cannot settle it and would raise a hard
% "did not converge" error. Newton on the residual F(s) = (u + K*s) - s solves
% the well-posed linear loop exactly. Reaching the analytic value therefore
% proves the Newton fallback (not the sweep) resolved the loop.
%=============================================================================

function r = solve_pos_feedback(u, K)
  evalin('base', 'clear(''nf_newton_out'')');
  d = struct();
  d.blocks = {...
    struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', u)), ...
    struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
      'params', struct('Inputs', '++')), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Gain', K)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'nf_newton_out', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 's', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  asserts.isequal(res.status, 'done'); % no hard algebraic-loop error
  vals = evalin('base', 'nf_newton_out');
  r = vals(end);
  evalin('base', 'clear(''nf_newton_out'')');
end

%=============================================================================
% Case 1: K = 2 (positive feedback): s = u / (1 - 2) = -u.
% Gauss-Seidel: s -> u -> 3u -> 7u -> ... diverges. Newton -> exact.
%=============================================================================
s1 = solve_pos_feedback(3.0, 2.0);
asserts.isapprox(s1, -3.0, 0, 1e-7);

%=============================================================================
% Case 2: stronger conditioning K = 5: s = u / (1 - 5) = -u/4.
%=============================================================================
s2 = solve_pos_feedback(3.0, 5.0);
asserts.isapprox(s2, -3.0 / 4.0, 0, 1e-7);

%=============================================================================
% Case 3: negative u, K = 3: s = u / (1 - 3) = -u/2.
%=============================================================================
s3 = solve_pos_feedback(-2.0, 3.0);
asserts.isapprox(s3, 1.0, 0, 1e-7);
%=============================================================================
