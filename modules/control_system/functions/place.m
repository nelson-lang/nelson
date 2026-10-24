%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = place(varargin)
  % Pole placement by state feedback (robust assignment).
  %
  %   K = place(A, B, p)
  %   [K, prec] = place(A, B, p)
  %   [K, prec, message] = place(A, B, p)
  %
  % Computes a gain matrix K such that the eigenvalues of (A - B*K) are the
  % entries of the vector p. Unlike acker (Ackermann's formula, single input
  % only), place supports a multi-input B and selects the closed-loop
  % eigenvectors so that the assignment is as insensitive as possible to
  % perturbations.
  %
  % The method is the rank-one update algorithm (method 0) described in
  % J. Kautsky, N. K. Nichols and P. Van Dooren, "Robust pole assignment in
  % linear state feedback", International Journal of Control, vol. 41, no. 5,
  % pp. 1129-1155, 1985.
  narginchk(3, 3)
  nargoutchk(0, 3)

  A = varargin{1};
  B = varargin{2};
  p = varargin{3};

  [n, rankB, U1] = placeValidateInputs(A, B, p);
  p = p(:);
  partner = placePairPoles(p, rankB);

  if (rankB >= n)
    % B has full row rank: any spectrum is reachable, so build a well
    % conditioned real matrix with the requested eigenvalues directly.
    Ac = placeRealSpectrum(p, partner, n);
  else
    ker = placeNullSpaces(A, U1, p, n, rankB);
    X = placeEigenvectors(ker, p, partner, n, rankB);
    Ac = real(X * diag(p) / X);
  end
  K = real(pinv(B) * (A - Ac));

  [prec, message] = placePrecision(A, B, K, p);
  if ~isempty(message) && nargout < 3
    warning('Nelson:control_system:place:badAccuracy', '%s', message);
  end

  varargout{1} = K;
  if nargout >= 2
    varargout{2} = prec;
  end
  if nargout >= 3
    varargout{3} = message;
  end
end
%=============================================================================
function [n, rankB, U1] = placeValidateInputs(A, B, p)
  if ~isnumeric(A) || ~isnumeric(B) || ~isnumeric(p)
    error('Nelson:control_system:place:invalidType', 'Matrices %s, %s and poles %s must be numeric.', 'A', 'B', 'p');
  end
  [mA, nA] = size(A);
  if (mA ~= nA)
    error('Nelson:expectedSquare', '%s must be square.', 'Matrix A');
  end
  [mB, ~] = size(B);
  if (mB ~= mA)
    error('Nelson:control_system:AAndBNumRowsMismatch', 'The number of rows in matrices %s and %s must be equal.', 'A', 'B');
  end
  n = nA;
  if (numel(p) ~= n)
    error('Nelson:control_system:dimagree:polesNeedDimensionMatrixA', 'Poles need to have the same dimension as matrix A')
  end
  rankB = rank(B);
  if (rankB == 0)
    error('Nelson:control_system:place:zeroB', '%s must have at least one nonzero column.', 'Matrix B');
  end
  [Qb, ~] = qr(B);
  U1 = Qb(:, rankB + 1:n);
end
%=============================================================================
function partner = placePairPoles(p, rankB)
  % Verify conjugate pairing, reject excessive multiplicity, return the index
  % of the conjugate partner of each pole (0 when the pole is real).
  n = numel(p);
  partner = zeros(n, 1);
  tolp = 1e-9 * max(1, max(abs(p)));
  for i = 1:n
    mult = sum(abs(p - p(i)) <= tolp);
    if (mult > rankB)
      error('Nelson:control_system:place:multiplicity', 'Pole multiplicity cannot exceed the rank of %s (%d).', 'B', rankB);
    end
  end
  used = false(n, 1);
  for i = 1:n
    if used(i)
      continue;
    end
    if (abs(imag(p(i))) <= tolp)
      used(i) = true;
      continue;
    end
    found = 0;
    for k = 1:n
      if (~used(k)) && (k ~= i) && (abs(p(k) - conj(p(i))) <= tolp)
        found = k;
        break;
      end
    end
    if (found == 0)
      error('Nelson:control_system:place:complexConjugate', 'Complex poles must be specified in conjugate pairs.');
    end
    partner(i) = found;
    partner(found) = i;
    used(i) = true;
    used(found) = true;
  end
end
%=============================================================================
function D = placeRealSpectrum(p, partner, n)
  % Real block-diagonal matrix whose eigenvalues are the entries of p: a
  % 1-by-1 block for each real pole and a [sigma w; -w sigma] block for each
  % conjugate pair.
  D = zeros(n, n);
  done = false(n, 1);
  for j = 1:n
    if done(j)
      continue;
    end
    if (partner(j) == 0)
      D(j, j) = real(p(j));
      done(j) = true;
    else
      k = partner(j);
      sigma = real(p(j));
      w = abs(imag(p(j)));
      D(j, j) = sigma;
      D(k, k) = sigma;
      D(j, k) = w;
      D(k, j) = -w;
      done(j) = true;
      done(k) = true;
    end
  end
end
%=============================================================================
function ker = placeNullSpaces(A, U1, p, n, rankB)
  % Orthonormal basis of the allowable eigenvector subspace of each pole,
  % i.e. the null space of U1' * (A - p(j) * I).
  ker = cell(n, 1);
  nFree = n - rankB;
  Id = eye(n);
  for j = 1:n
    if (nFree <= 0)
      ker{j} = Id;
      continue;
    end
    M = U1' * (A - p(j) * Id);
    [Qm, ~] = qr(M');
    ker{j} = Qm(:, nFree + 1:n);
  end
end
%=============================================================================
function X = placeEigenvectors(ker, p, partner, n, rankB)
  % KNV method 0: rank-one updates that maximise the smallest singular value
  % (best conditioning) of the eigenvector matrix X. A real pole keeps a real
  % eigenvector; a complex pole is updated once and its conjugate partner
  % mirrors it so that A - B*K stays real.
  tolp = 1e-9 * max(1, max(abs(p)));
  isReal = abs(imag(p)) <= tolp;
  X = zeros(n, n);
  for j = 1:n
    s = ker{j}(:, 1);
    X(:, j) = s / norm(s);
  end
  X = placeEnforceConjugate(X, p, partner);
  if (rankB <= 1)
    return;
  end
  Xbest = X;
  bestS = min(svd(X));
  maxIter = 60;
  for iter = 1:maxIter
    for j = 1:n
      if (partner(j) > 0) && (imag(p(j)) < 0)
        continue; % conjugate partner is set by its primary
      end
      idx = [1:j - 1, j + 1:n];
      [Q, ~] = qr(X(:, idx));
      q = Q(:, end);
      if isReal(j)
        q = real(q);
      end
      Sj = ker{j};
      y = Sj * (Sj' * q);
      ny = norm(y);
      if (ny > 1e-12)
        X(:, j) = y / ny;
      end
      X = placeEnforceConjugate(X, p, partner);
    end
    s = min(svd(X));
    if (s > bestS)
      bestS = s;
      Xbest = X;
    end
  end
  X = Xbest;
end
%=============================================================================
function X = placeEnforceConjugate(X, p, partner)
  n = numel(p);
  for j = 1:n
    if (partner(j) > 0) && (imag(p(j)) > 0)
      X(:, partner(j)) = conj(X(:, j));
    end
  end
end
%=============================================================================
function [prec, message] = placePrecision(A, B, K, p)
  clPoles = eig(A - B * K);
  n = numel(p);
  used = false(n, 1);
  relerr = zeros(n, 1);
  for i = 1:n
    best = inf;
    bj = 0;
    for k = 1:n
      if used(k)
        continue;
      end
      d = abs(p(i) - clPoles(k));
      if (d < best)
        best = d;
        bj = k;
      end
    end
    used(bj) = true;
    relerr(i) = best / max(1, abs(p(i)));
  end
  worst = max([relerr; eps]);
  prec = -log10(worst);
  message = '';
  if (worst > 1e-3)
    message = 'Pole locations could not be matched to the requested accuracy.';
  end
end
%=============================================================================
