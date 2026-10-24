%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: a direct-feedthrough discrete block (difference, the detect*
% family) whose OUTPUT reads the CURRENT input must NOT depend on where it is
% declared relative to its source.
%
% The fixed-step engine runs OUTPUT-phase blocks in DECLARATION order, so an
% OUTPUT-phase feedthrough block declared BEFORE its source ran before that
% source and read a not-yet-computed input (reset to 0 at each step start),
% emitting garbage: difference produced -u[k-1] instead of u[k]-u[k-1]. These
% blocks are now produced in the topologically-ordered ALGEBRAIC phase (like
% derivative), so the source is guaranteed to have run first and the result is
% declaration-order independent.
%=============================================================================
function j = mkmodel(misordered, blkType, blkParams)
  df = struct('id', 'df', 'type', blkType, 'inputs', 1, 'outputs', 1, 'params', blkParams);
  r = struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 1));
  sc = struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
  if misordered
    blocks = {df, r, sc}; % feedthrough block declared BEFORE its source
  else
    blocks = {r, df, sc}; % source declared first (the well-ordered baseline)
  end
  d = struct();
  d.blocks = blocks;
  d.connections = {...
    struct('from', 'r', 'to', 'df', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'df', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
  d.solver = 'discrete';
  d.duration = 1.0;
  d.sampleTime = 0.1;
  j = jsonencode(d);
end
%=============================================================================
% difference on a unit-slope ramp sampled at 0.1: u[k] = 0.1 k, so the backward
% difference is a constant 0.1 for k >= 1 (and 0 at k = 0). The mis-ordered bug
% produced -u[k-1] (0, 0, -0.1, -0.2, ...): negative and growing.
vMis = jsondecode(__nflow_simulate__(mkmodel(true, 'difference', struct('ICPrevInput', 0)))).scopes(1).series(:);
vOrd = jsondecode(__nflow_simulate__(mkmodel(false, 'difference', struct('ICPrevInput', 0)))).scopes(1).series(:);
n = min(numel(vMis), numel(vOrd));
assert_istrue(n >= 10);
% Declaration order must not matter: the two trajectories are byte-for-byte equal.
assert_istrue(max(abs(vMis(1:n) - vOrd(1:n))) < 1e-12);
% And the value is the correct backward difference, not the -u[k-1] garbage.
assert_isapprox(vMis(1), 0.0, 1e-12);
assert_istrue(max(abs(vMis(2:n) - 0.1)) < 1e-9);
assert_istrue(min(vMis(1:n)) > -1e-9); % never the negative garbage
%=============================================================================
% detectChange on the same (always changing) ramp: 0 at k = 0, then 1 for every
% k >= 1. The mis-ordered bug read 0 in OUTPUT, so it compared 0 against the
% latched previous input and missed the first transition (0 at k = 1).
cMis = jsondecode(__nflow_simulate__(mkmodel(true, 'detectChange', struct('InitialCondition', 0)))).scopes(1).series(:);
cOrd = jsondecode(__nflow_simulate__(mkmodel(false, 'detectChange', struct('InitialCondition', 0)))).scopes(1).series(:);
nc = min(numel(cMis), numel(cOrd));
assert_istrue(nc >= 10);
assert_istrue(max(abs(cMis(1:nc) - cOrd(1:nc))) < 1e-12);
assert_isapprox(cMis(1), 0.0, 1e-12);
assert_istrue(max(abs(cMis(2:nc) - 1.0)) < 1e-12); % every step after the first is a change
%=============================================================================
