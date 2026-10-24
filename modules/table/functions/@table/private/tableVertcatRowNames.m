%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableVertcatRowNames(namesA, nA, namesB, nB)
  % Row names of [A; B]: none when neither table has row names, otherwise
  % the rows of a table without row names are named Row<k> after their row
  % number in the result, and a row name given in both tables is an error.
  hasA = tableHasRowNames(namesA, nA);
  hasB = tableHasRowNames(namesB, nB);
  if ~hasA && ~hasB
    names = {};
    return
  end
  if hasA
    namesA = reshape(namesA, [], 1);
  else
    namesA = tableDefaultRowNames(1:nA, namesB);
  end
  if hasB
    namesB = reshape(namesB, [], 1);
  else
    namesB = tableDefaultRowNames(nA + (1:nB), namesA);
  end
  [isDuplicate, idx] = ismember(namesB, namesA);
  if any(isDuplicate)
    error('Nelson:table:vertcat:duplicateRowNames', _('Duplicate table row name: ''%s''.'), namesA{idx(find(isDuplicate, 1))});
  end
  names = [namesA; namesB];
end
%=============================================================================
