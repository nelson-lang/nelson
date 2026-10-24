%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function normals = computeIsonormals(x, y, z, V, vertices, negate)
  [gx, gy, gz] = volumeGradient(double(V), double(x), double(y), double(z));
  nx = -interp3(x, y, z, gx, vertices(:, 1), vertices(:, 2), vertices(:, 3), 'linear');
  ny = -interp3(x, y, z, gy, vertices(:, 1), vertices(:, 2), vertices(:, 3), 'linear');
  nz = -interp3(x, y, z, gz, vertices(:, 1), vertices(:, 2), vertices(:, 3), 'linear');
  normals = [nx(:), ny(:), nz(:)];
  if negate
    normals = -normals;
  end
end
%=============================================================================
