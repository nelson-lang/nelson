%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ss2tf(varargin)
  % Transform state space model to transfer function
  % [num, den] = ss2tf(A, B, C, D)
  % [num, den] = ss2tf(A, B, C, D, iu)

  narginchk(4, 5);
  nargoutchk(0, 2);

  % Extract inputs
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  D = varargin{4};
  if isempty(A)
    % A static gain takes its input and output counts from D.
    [B, C] = staticGainShapes(B, C, D);
  end

  % Validate state-space matrices
  msg = checkABCDE(A, B, C, D);
  if ~isempty(msg)
    error(msg.identifier, msg.message);
  end

  nu = size(B, 2);

  % Validate and handle input index (iu)
  if nargin == 4
    if nu <= 1
      iu = 1;
    else
      error('Nelson:control_system:invalidType', '%s expected.', 'Single input system');
    end
  else
    iu = varargin{5};
    validateInputIndex(iu, nu);
  end

  % Compute transfer function coefficients
  [numerator, denominator] = computeTransferFunctionCoefficients(A, B, C, D, iu);

  % Only real parts for coefficients are desired
  numerator = real(numerator);
  denominator = real(denominator);

  % Output results
  varargout{1} = numerator;
  if nargout > 1
    varargout{2} = denominator;
  end
end
%=============================================================================
function validateInputIndex(iu, nu)
  % Validate input index (iu)
  if ~isnumeric(iu) || ~isscalar(iu) || iu < 1 || iu > nu
    error('Nelson:control_system:invalidIndex', 'Index out of bounds or not a scalar.');
  end
end
%=============================================================================
function [numerator, denominator] = computeTransferFunctionCoefficients(A, B, C, D, iu)
  denominator = poly(A);
  if isempty(D)
    numerator = D;
  elseif isempty(A)
    numerator = D(:, iu);
  else
    numerator = computeNumerator(A, B, C, D, iu);
  end
end
%=============================================================================
function [B, C] = staticGainShapes(B, C, D)
  if isempty(B)
    B = zeros(0, size(D, 2));
  end
  if isempty(C)
    C = zeros(size(D, 1), 0);
  end
end
%=============================================================================
function V = computeNumerator(A, B, C, D, iu)
  mc = size(D, 1);
  nc = size(A, 1);
  V = zeros(mc, nc + 1);
  for k = 1:mc
    V(k, :) = channelNumerator(A, B(:, iu), C(k, :), D(k, iu));
  end
end
%=============================================================================
function num = channelNumerator(A, b, c, d)
  % Numerator is gain * prod(s - zeros), so structural zeros stay exact.
  n = size(A, 1);
  if d ~= 0
    num = d * poly(eig(A - b * (c / d)));
    return
  end
  num = zeros(1, n + 1);
  rows = zeros(n, n);
  w = c;
  for r = 1:n
    g = w * b;
    if abs(g) > 10 * n * eps * norm(w) * norm(b)
      % Zeros are the eigenvalues of the zero dynamics on the kernel of c, c*A, ...
      F = A - b * ((w * A) / g);
      rows(r, :) = w;
      N = null(rows(1:r, :));
      num(r + 1:end) = g * poly(eig(N' * F * N));
      return
    end
    rows(r, :) = w;
    w = w * A;
  end
end
%=============================================================================
