%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z] = validateIsosurfaceGrid(X, Y, Z, V)
  validateIsosurfaceGridDataTypes(X, Y, Z);
  if isvector(X) && isvector(Y) && isvector(Z)
    x = double(X(:).');
    y = double(Y(:).');
    z = double(Z(:).');
  else
    [x, y, z] = isosurfaceGridArraysToVectors(X, Y, Z, V);
  end
  if numel(x) ~= size(V, 2) || numel(y) ~= size(V, 1) || numel(z) ~= size(V, 3)
    error('Nelson:isosurface:gridMismatch', 'Grid vectors must match volume dimensions.');
  end
  validateIsosurfaceGridVector(x);
  validateIsosurfaceGridVector(y);
  validateIsosurfaceGridVector(z);
end
%=============================================================================
