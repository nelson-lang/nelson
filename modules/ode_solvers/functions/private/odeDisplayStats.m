%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function odeDisplayStats(problem, stats)
  if ~strcmpi(problem.options.Stats, 'on')
    return
  end
  fprintf('%d successful steps\n', stats.nsteps);
  fprintf('%d failed attempts\n', stats.nfailed);
  fprintf('%d function evaluations\n', stats.nfevals);
end
%=============================================================================
