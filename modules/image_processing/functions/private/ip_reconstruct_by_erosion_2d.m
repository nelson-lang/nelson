%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_reconstruct_by_erosion_2d(marker, mask, conn)
  previous = marker;
  nhood = ip_connectivity_nhood_2d(conn);
  maxIterations = numel(marker) + 1;
  for k = 1:maxIterations
    current = max(imerode(previous, nhood), mask);
    if isequal(current, previous)
      out = current;
      return
    end
    previous = current;
  end
  out = previous;
end
%=============================================================================
