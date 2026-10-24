%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [isovalue, colors] = parseLevelOrColors(value, V)
  colors = [];
  if isnumeric(value) && isreal(value) && isequal(size(value), size(V)) && ~isscalar(value)
    isovalue = [];
    colors = validateIsosurfaceColors(value, V);
  else
    isovalue = validateIsosurfaceLevel(value);
  end
end
%=============================================================================
