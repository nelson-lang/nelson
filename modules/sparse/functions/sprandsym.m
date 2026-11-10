%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = sprandsym(n, density, rc, kind)
  % random sparse symmetric matrix, optionally positive definite with a given reciprocal condition
  narginchk(1, 4);
  if nargin == 1
    S = n;
    if ~ismatrix(S) || size(S, 1) ~= size(S, 2)
      error('Nelson:sprandsym:invalidInput', 'Input must be a square matrix or a size with a density.');
    end
    [i, j] = find(tril(S));
    T = sparse(i, j, randn(numel(i), 1), size(S, 1), size(S, 2));
    R = T + tril(T, -1).';
    return
  end
  validSize = isnumeric(n) && isscalar(n) && n >= 0 && n == fix(n);
  if ~validSize || ~(isnumeric(density) && isscalar(density) && density >= 0 && density <= 1)
    error('Nelson:sprandsym:invalidInput', 'Input must be a square matrix or a size with a density.');
  end
  if nargin < 3 || isempty(rc)
    T = tril(sprandn(n, n, density));
    R = T + tril(T, -1).';
    return
  end
  if nargin > 3 && ~(isequal(kind, 1) || isequal(kind, 2))
    error('Nelson:sprandsym:unsupportedKind', 'Only kinds 1 and 2 are supported.');
  end
  R = rotatedSpectrum(n, density, rc);
end
%=============================================================================
function R = rotatedSpectrum(n, density, rc)
  % positive definite matrix from random plane rotations of a diagonal with the requested eigenvalues
  if isscalar(rc)
    if ~(rc > 0 && rc <= 1)
      error('Nelson:sprandsym:invalidCondition', 'The reciprocal condition must be in (0, 1].');
    end
    if n > 1
      d = rc .^ ((0:n - 1).' / (n - 1));
    else
      d = ones(n, 1);
    end
  else
    d = double(rc(:));
    if numel(d) ~= n || any(d <= 0)
      error('Nelson:sprandsym:invalidCondition', 'The reciprocal condition must be in (0, 1].');
    end
  end
  R = sparse(1:n, 1:n, d, n, n);
  if n < 2
    return
  end
  target = density * n * n;
  for count = 1:4 * n * n
    if nnz(R) >= target
      break
    end
    pair = randperm(n, 2);
    angle = 2 * pi * rand();
    G = [cos(angle), sin(angle); -sin(angle), cos(angle)];
    R(pair, :) = G * R(pair, :);
    R(:, pair) = R(:, pair) * G.';
  end
  R = (R + R.') / 2;
end
%=============================================================================
