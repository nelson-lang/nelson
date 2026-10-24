%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TF = ismissing(T)
  TF = false(height(T), width(T));
  names = T.Properties.VariableNames;
  for k = 1:length(names)
    m = ismissing(T.(names{k}));
    if isvector(m)
      TF(:, k) = m(:);
    else
      TF(:, k) = any(reshape(m, height(T), []), 2);
    end
  end
end
%=============================================================================
