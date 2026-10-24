%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function loc = __class_locations__(values, requested)
  loc = zeros(length(values), 1);
  for k = 1:length(values)
    for j = 1:length(requested)
      if isequal(values(k), requested(j))
        loc(k) = j;
        break
      end
      if iscell(values) && iscell(requested) && strcmp(values{k}, requested{j})
        loc(k) = j;
        break
      end
    end
  end
end
%=============================================================================
