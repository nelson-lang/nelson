%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableRenameRepeated(names, indices, maxLength)
  % names = source(indices), variable or row names. When an index is
  % repeated, its first occurrence keeps the name and the next ones get a
  % _k suffix that no other name of the list uses
  % ({'B', 'B', 'B_1'} -> {'B', 'B_2', 'B_1'}). Without maxLength, names are
  % not truncated (row names).
  n = numel(indices);
  if n < 2
    return
  end
  sorted = sortrows([indices(:), (1:n)']);
  repeated = false(1, n);
  repeated(sorted(2:end, 2)) = diff(sorted(:, 1)) == 0;
  if ~any(repeated)
    return
  end
  if nargin < 3
    maxLength = max(cellfun('length', names)) + 1 + length(int2str(n));
  end
  names = nelson.lang.makeUniqueStrings(names, reshape(repeated, size(names)), maxLength);
end
%=============================================================================
