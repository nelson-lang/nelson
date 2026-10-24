%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateNames(obj, names, message)
  if isempty(names)
    return
  end
  if ~iscellstr(names)
    error('Nelson:table:invalidVariableNames', message);
  end
  for k = 1:length(names)
    if isempty(names{k})
      error('Nelson:table:invalidVariableNames', message);
    end
  end
  if length(unique(names)) ~= length(names)
    error('Nelson:table:duplicateVariableNames', message);
  end
end
%=============================================================================
