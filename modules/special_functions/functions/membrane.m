%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% membrane computed with a finite-difference eigensolver of the
% L-shaped membrane. The L-shape mask, the smallest eigenfunctions (k = 1 is a
% single positive bump, higher modes gain nodal lines) and the shift-invert
% solve were checked with an independent scipy reimplementation. The following
% still need to be confirmed against the reference once the build is usable:
%   - orientation of the L (which quadrant is removed, and any flipud/rot90),
%   - the sign/normalisation convention,
%   - that eigs(A, k, 0) (shift-invert around zero) is accepted here,
%   - performance for n = 100 (a 201-by-201 grid).
%=============================================================================
function varargout = membrane(k, n, m, s)
  narginchk(0, 4);
  nargoutchk(0, 1);

  if nargin < 1 || isempty(k)
    k = 1;
  end
  if nargin < 2 || isempty(n)
    n = 15;
  end
  % m and s are the series-term counts of the classic method; this
  % finite-difference solver does not use them but accepts them for
  % call-signature compatibility.
  k = round(double(k));
  n = round(double(n));
  if ~isscalar(k) || k < 1
    error('Nelson:membrane:invalidIndex', 'The eigenfunction index must be a positive integer.');
  end
  if ~isscalar(n) || n < 1
    error('Nelson:membrane:invalidSize', 'The grid parameter must be a positive integer.');
  end

  L = lShapedMembrane(k, n);

  if nargout == 0
    surf(L);
    shading('interp');
  else
    varargout{1} = L;
  end
end
%=============================================================================
function L = lShapedMembrane(k, n)
  N = 2 * n + 1;
  centre = n + 1;
  coords = linspace(-1, 1, N);
  [colIndex, rowIndex] = meshgrid(1:N, 1:N);
  X = coords(colIndex);
  Y = coords(rowIndex);

  % The L-shaped domain is the square [-1, 1] x [-1, 1] with one open quadrant
  % removed (the X < 0, Y > 0 corner, to match the reference orientation); the
  % reentrant corner sits at the centre.
  removed = (X < 0) & (Y > 0);
  % The inner edges of the notch are part of the boundary (value zero there).
  reentrant = ((colIndex == centre) & (Y >= 0)) | ((rowIndex == centre) & (X <= 0));
  outerFrame = (rowIndex == 1) | (rowIndex == N) | (colIndex == 1) | (colIndex == N);
  unknown = ~removed & ~reentrant & ~outerFrame;

  nbUnknown = nnz(unknown);
  if nbUnknown == 0
    L = zeros(N, N);
    return
  end

  indexMap = zeros(N, N);
  linearUnknown = find(unknown);
  indexMap(linearUnknown) = 1:nbUnknown;
  [ri, cj] = ind2sub([N, N], linearUnknown);

  A = buildLaplacian(ri, cj, indexMap, N, nbUnknown);
  vector = smallestEigenvector(A, k);

  L = zeros(N, N);
  L(unknown) = vector;
  peak = max(abs(L(:)));
  if peak > 0
    % Symmetric modes can have equal positive and negative extrema. Choose
    % the first peak within the solver tolerance so roundoff cannot flip the
    % sign between calls.
    atIndex = find(abs(L(:)) >= peak * (1 - 1e-10), 1);
    L = (L / peak) * sign(L(atIndex));
  end
end
%=============================================================================
function A = buildLaplacian(ri, cj, indexMap, N, nbUnknown)
  % Five-point Laplacian over the interior unknowns with homogeneous Dirichlet
  % conditions: a neighbour that is a boundary (zero) point simply drops out.
  rows = (1:nbUnknown)';
  cols = (1:nbUnknown)';
  vals = 4 * ones(nbUnknown, 1);
  steps = [-1, 0; 1, 0; 0, -1; 0, 1];
  for t = 1:size(steps, 1)
    ni = ri + steps(t, 1);
    nj = cj + steps(t, 2);
    inside = ni >= 1 & ni <= N & nj >= 1 & nj <= N;
    neighbour = zeros(nbUnknown, 1);
    neighbour(inside) = indexMap(sub2ind([N, N], ni(inside), nj(inside)));
    isUnknown = neighbour > 0;
    rows = [rows; find(isUnknown)];
    cols = [cols; neighbour(isUnknown)];
    vals = [vals; -ones(nnz(isUnknown), 1)];
  end
  A = sparse(rows, cols, vals, nbUnknown, nbUnknown);
end
%=============================================================================
function vector = smallestEigenvector(A, k)
  % The k-th eigenfunction is the eigenvector of the k-th smallest eigenvalue.
  % The operator is positive definite, so a shift-invert around zero returns the
  % smallest eigenvalues quickly (a plain smallest-magnitude search converges
  % far too slowly on this grid).
  kk = min(k, size(A, 1));
  [V, D] = eigs(A, kk, 0);
  [~, order] = sort(real(diag(D)), 'ascend');
  V = V(:, order);
  vector = real(V(:, min(k, size(V, 2))));
end
%=============================================================================
