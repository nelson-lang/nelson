%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function colors = validateIsosurfaceColors(colors, V)
  if ~isnumeric(colors) || ~isreal(colors) || ~isequal(size(colors), size(V))
    error('Nelson:isosurface:colorMismatch', 'Color data must have the same size as the volume data.');
  end
  colors = double(colors);
end
%=============================================================================
