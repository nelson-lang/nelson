%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableDefaultRowNames(rows, takenNames)
  % Row<k> for each row number k in rows, with a _<n> suffix when that name
  % is already in takenNames or given to an earlier row.
  if isempty(rows)
    names = cell(0, 1);
    return
  end
  names = reshape(cellstr(compose('Row%d', rows(:))), [], 1);
  clash = find(ismember(names, takenNames));
  for k = reshape(clash, 1, [])
    base = names{k};
    suffix = 1;
    while any(strcmp(takenNames, names{k})) || any(strcmp(names([1:k - 1, k + 1:end]), names{k}))
      names{k} = sprintf('%s_%d', base, suffix);
      suffix = suffix + 1;
    end
  end
end
%=============================================================================
