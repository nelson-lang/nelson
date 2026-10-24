%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solout = odeAppendSolutions(left, right)
  right = odeSolutionData(right);
  solout = left;
  solout.x = [left.x, right.x(2:length(right.x))];
  solout.y = [left.y, right.y(:, 2:size(right.y, 2))];
  if isfield(left, 'extdata') && isfield(left.extdata, 'slopes') ...
      && isfield(right, 'extdata') && isfield(right.extdata, 'slopes')
    solout.extdata.slopes = [left.extdata.slopes, right.extdata.slopes(:, 2:size(right.extdata.slopes, 2))];
  end
  solout.xe = [left.xe, right.xe];
  solout.ye = [left.ye, right.ye];
  solout.ie = [left.ie, right.ie];
  if isempty(solout.xe)
    solout = rmfield(solout, {'xe', 'ye', 'ie'});
  end
  solout.stats.nsteps = left.stats.nsteps + right.stats.nsteps;
  solout.stats.nfailed = left.stats.nfailed + right.stats.nfailed;
  solout.stats.nfevals = left.stats.nfevals + right.stats.nfevals;
  solout.stats.tfinal = right.stats.tfinal;
end
%=============================================================================
