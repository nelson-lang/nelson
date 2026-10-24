%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = rectint(A, B)
  % Area of intersection of rectangles.
  %   out = rectint(A, B)
  % A is an M-by-4 matrix and B an N-by-4 matrix, each row a rectangle
  % [x y width height]. out(i, j) is the area of the intersection of A(i, :)
  % and B(j, :), so out is M-by-N.
  narginchk(2, 2);
  if ~isnumeric(A) || ~isnumeric(B) || size(A, 2) ~= 4 || size(B, 2) ~= 4
    error('Nelson:rectint:invalidInput', 'A and B must be numeric matrices with four columns.');
  end
  A = double(A);
  B = double(B);
  m = size(A, 1);
  n = size(B, 1);
  % Left/right and bottom/top edges of every rectangle.
  aLeft = A(:, 1);
  aRight = A(:, 1) + A(:, 3);
  aBottom = A(:, 2);
  aTop = A(:, 2) + A(:, 4);
  bLeft = B(:, 1)';
  bRight = (B(:, 1) + B(:, 3))';
  bBottom = B(:, 2)';
  bTop = (B(:, 2) + B(:, 4))';
  % Overlap length along each axis, clamped at zero, broadcast to M-by-N.
  overlapX = max(0, min(repmat(aRight, 1, n), repmat(bRight, m, 1)) ...
    - max(repmat(aLeft, 1, n), repmat(bLeft, m, 1)));
  overlapY = max(0, min(repmat(aTop, 1, n), repmat(bTop, m, 1)) ...
    - max(repmat(aBottom, 1, n), repmat(bBottom, m, 1)));
  out = overlapX .* overlapY;
end
%=============================================================================
