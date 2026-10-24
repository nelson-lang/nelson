%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z] = isosurfaceGridArraysToVectors(X, Y, Z, V)
  if ~isequal(size(X), size(V)) || ~isequal(size(Y), size(V)) || ~isequal(size(Z), size(V))
    error('Nelson:volumeGridVectors:invalidGridArrays', 'Grid arrays must have the same size as V.');
  end
  x = double(squeeze(X(1, :, 1))).';
  y = double(squeeze(Y(:, 1, 1))).';
  z = double(squeeze(Z(1, 1, :))).';
end
%=============================================================================
