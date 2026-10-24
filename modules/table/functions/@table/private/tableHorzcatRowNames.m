%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [names, rows] = tableHorzcatRowNames(names, namesB, nRows)
  % Row names of [A, B], where names are the row names of A: B gives them
  % when A has none. When both have row names, rows is the order of the
  % rows of B matching the row names of A ([] when B is already in order).
  rows = [];
  if ~tableHasRowNames(namesB, nRows)
    return
  end
  if ~tableHasRowNames(names, nRows)
    names = reshape(namesB, [], 1);
    return
  end
  [isFound, rows] = ismember(names, namesB);
  if ~all(isFound)
    error('Nelson:table:horzcatRowNamesMismatch', _('All tables being horizontally concatenated must have the same row names.'));
  end
  if isequal(reshape(rows, [], 1), (1:nRows)')
    rows = [];
  end
end
%=============================================================================
