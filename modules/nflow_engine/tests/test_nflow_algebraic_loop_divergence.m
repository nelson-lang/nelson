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
% A SINGULAR algebraic loop -- one with no unique solution -- must fail with a
% clear diagnostic, not spin forever or return a wrong answer.
%   s = c + g, g = 1*s  =>  s = c + s  =>  0 = c,
% which has no solution for c != 0. The fixed-point residual F(s) = (c + s) - s
% = c is constant (nonzero), so its Jacobian is singular and the Newton fallback
% cannot solve it either. The algebraic solve must give up and report
% "algebraic loop did not converge", listing the blocks of the offending loop in
% errorBlocks (the failing strongly-connected component, not the whole diagram).
%
% NOTE: a merely non-contractive but WELL-POSED loop (e.g. s = c + 2*s, whose
% unique solution is s = -c) is no longer an error -- the Newton fallback solves
% it exactly. That case is covered by test_nflow_algebraic_loop_newton. This
% test now pins the genuinely unsolvable (singular) case.
% Complements test_nflow_algebraic_loop (which covers the convergent case).
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 1)), ...
  struct('id', 's', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
    'params', struct('Inputs', '++')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 1)), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ydiv', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'c', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 's', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.5;
d.solver = 'discrete';
d.variables = struct();
%=============================================================================
r = jsondecode(__nflow_simulate__(jsonencode(d)));
% The run is aborted with the non-convergence diagnostic.
assert_isequal(r.status, 'error');
assert_istrue(~isempty(strfind(r.reason, 'did not converge')));
% The offending loop's blocks are reported (the sum/gain cycle), not empty.
assert_istrue(isfield(r, 'errorBlocks'));
assert_istrue(~isempty(r.errorBlocks));
ids = r.errorBlocks;
if ~iscell(ids)
  ids = cellstr(ids);
end
assert_istrue(any(strcmp(ids, 's')) || any(strcmp(ids, 'g')));
%=============================================================================
