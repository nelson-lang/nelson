%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z] = volumeGridVectors(X, Y, Z, V)
  if nargin == 1
    V = X;
    x = 1:size(V, 2);
    y = 1:size(V, 1);
    z = 1:size(V, 3);
    return
  end
  if isvector(X) && isvector(Y) && isvector(Z)
    x = double(X(:).');
    y = double(Y(:).');
    z = double(Z(:).');
    return
  end
  if ~isequal(size(X), size(Y)) || ~isequal(size(X), size(Z)) || ~isequal(size(X), size(V))
    error('Nelson:volumeGridVectors:invalidGridArrays', 'Grid arrays must have the same size as V.');
  end
  x = double(squeeze(X(1, :, 1))).';
  y = double(squeeze(Y(:, 1, 1))).';
  z = double(squeeze(Z(1, 1, :))).';
end
%=============================================================================
