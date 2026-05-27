%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [values, ia, ic] = tableUniqueStable(valuesIn)
  values = {};
  ia = [];
  ic = zeros(size(valuesIn));
  for k = 1:length(valuesIn)
    found = find(strcmp(values, valuesIn{k}), 1);
    if isempty(found)
      values{end + 1} = valuesIn{k};
      ia(end + 1) = k;
      found = length(values);
    end
    ic(k) = found;
  end
  values = values(:);
  ia = ia(:);
end
%=============================================================================
