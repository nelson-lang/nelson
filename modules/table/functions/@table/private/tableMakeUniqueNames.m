%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = tableMakeUniqueNames(names)
  names = tableMakeValidName(names);
  used = {};
  for k = 1:length(names)
    base = names{k};
    candidate = base;
    suffix = 1;
    while any(strcmp(used, candidate))
      candidate = [base, '_', num2str(suffix)];
      suffix = suffix + 1;
    end
    names{k} = candidate;
    used{end + 1} = candidate;
  end
end
%=============================================================================
