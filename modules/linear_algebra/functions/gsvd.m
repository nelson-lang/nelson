%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = gsvd(A, B, option)
  % generalized singular value decomposition A = U*C*X', B = V*S*X' with C'*C + S'*S = I
  narginchk(2, 3);
  nargoutchk(0, 5);
  if ~(isfloat(A) && isfloat(B))
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  if size(A, 2) ~= size(B, 2)
    error('Nelson:linalg:pageColumnsMismatch', 'The two inputs must have the same number of columns.');
  end
  econ = false;
  if nargin > 2
    isEcon = (ischar(option) || isstring(option)) && strcmpi(char(option), 'econ');
    if ~(isEcon || isequal(option, 0))
      error('Nelson:gsvd:invalidOption', 'Option must be 0 or ''econ''.');
    end
    econ = true;
  end
  A = full(A);
  B = full(B);
  [m, p] = size(A);
  n = size(B, 1);
  % rank-revealing QR of the stacked matrix, then a CS decomposition of its orthogonal factor
  [Q, R, perm] = qr([A; B], 0);
  d = abs(diag(R));
  if isempty(d)
    q = 0;
  else
    q = sum(d > max(m + n, p) * eps(d(1)));
  end
  QA = Q(1:m, 1:q);
  QB = Q(m + 1:end, 1:q);
  [~, ~, W] = svd(QA);
  if size(W, 2) ~= q
    W = eye(q, 'like', Q);
  end
  W = W(:, end:-1:1);
  CA = QA * W;
  SB = QB * W;
  offset = max(0, q - m);
  ka = q - offset;
  kb = min(n, q);
  if nargout <= 1
    c = [zeros(offset, 1, 'like', d); sqrt(sum(abs(CA(:, offset + 1:q)) .^ 2, 1)).'];
    s = [sqrt(sum(abs(SB(:, 1:kb)) .^ 2, 1)).'; zeros(q - kb, 1, 'like', d)];
    varargout{1} = sort(c ./ s);
    return
  end
  % largest columns first so that a nearly zero column cannot spoil the others
  [U, RA] = qr(CA(:, q:-1:offset + 1));
  U(:, 1:ka) = U(:, ka:-1:1);
  [V, RB] = qr(SB(:, 1:kb));
  c = flipud(diag(RA(1:ka, 1:ka)));
  s = diag(RB(1:kb, 1:kb));
  signA = sign(c);
  signA(signA == 0) = 1;
  signB = sign(s);
  signB(signB == 0) = 1;
  U(:, 1:ka) = U(:, 1:ka) .* signA.';
  V(:, 1:kb) = V(:, 1:kb) .* signB.';
  C = zeros(m, q, 'like', d);
  C(sub2ind([m, q], 1:ka, offset + (1:ka))) = abs(c);
  S = zeros(n, q, 'like', d);
  S(sub2ind([n, q], 1:kb, 1:kb)) = abs(s);
  Rfull = zeros(q, p, 'like', R);
  Rfull(:, perm) = R(1:q, :);
  X = Rfull' * W;
  if econ
    U = U(:, 1:min(m, q));
    C = C(1:min(m, q), :);
    V = V(:, 1:kb);
    S = S(1:kb, :);
  end
  varargout = {U, V, X, C, S};
  varargout = varargout(1:max(nargout, 1));
end
%=============================================================================
