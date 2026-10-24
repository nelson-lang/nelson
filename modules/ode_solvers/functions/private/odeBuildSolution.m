%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeBuildSolution(problem, x, y, slopes, xe, ye, ie, stats)
  extdata = struct();
  extdata.slopes = slopes;
  extdata.odefun = problem.odefun;
  extdata.options = problem.options;
  extdata.yp0 = problem.yp0;
  if isfield(problem, 'storeRequestedOutputOnly') && problem.storeRequestedOutputOnly
    extdata.requestedOutputOnly = true;
  end
  idata = struct();
  idata.f3d = [];
  idata.idxNonNegative = problem.options.NonNegative;
  if isempty(xe)
    sol = struct('solver', problem.solver, 'extdata', extdata, ...
      'x', x, 'y', y, 'stats', stats, 'idata', idata);
  else
    sol = struct('solver', problem.solver, 'extdata', extdata, ...
      'x', x, 'y', y, 'xe', xe, 'ye', ye, 'ie', ie, ...
      'stats', stats, 'idata', idata);
  end
end
%=============================================================================
