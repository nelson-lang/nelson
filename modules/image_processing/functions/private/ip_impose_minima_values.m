%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_impose_minima_values(values, mask, model)
  if ~any(mask(:))
    out = model;
    return
  end
  [markerValue, floorValue] = ip_imimposemin_levels(values, model);
  result = values;
  result(mask) = markerValue;
  result(~mask) = max(result(~mask), floorValue);
  out = ip_cast_like(result, model);
end
%=============================================================================
