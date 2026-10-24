%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [vertices, faces] = cuboidPatchData(xlim, ylim, zlim)
  x0 = xlim(1); x1 = xlim(2);
  y0 = ylim(1); y1 = ylim(2);
  z0 = zlim(1); z1 = zlim(2);
  vertices = [x0 y0 z0; x1 y0 z0; x1 y1 z0; x0 y1 z0; ...
    x0 y0 z1; x1 y0 z1; x1 y1 z1; x0 y1 z1];
  faces = [1 2 3 4; 5 8 7 6; 1 5 6 2; 2 6 7 3; 3 7 8 4; 4 8 5 1];
end
%=============================================================================
