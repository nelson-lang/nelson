%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = condest(varargin)
  nargoutchk(0, 2);
  narginchk(1, 2);

  A = varargin{1};
  if (~isnumeric(A) || ndims(A) ~= 2)
    error('Nelson:condest:invalidInput', '%s expected.', 'An numeric 2-D matrix');
  end
  if (size(A, 1) ~= size(A, 2))
    error('Nelson:linear_algebra:squareMatrixExpected', '%s expected.', 'Square matrix');
  end

  n = size(A, 1);
  valueClass = class(A);
  if (n == 0)
    c = zeros(valueClass);
    v = zeros(0, 1, valueClass);
    varargout{1} = c;
    if (nargout > 1)
      varargout{2} = v;
    end
    return;
  end

  if (nargin == 2)
    t = varargin{2};
    if (isempty(t))
      t = min(n, 2);
    elseif (~isnumeric(t) || ~isscalar(t))
      error('Nelson:condest:TOutOfRange', 'T must be an integer between 1 and N = MAX(SIZE(A)).');
    else
      t = abs(t);
    end
    if (~isscalar(t) || t < 1 || floor(t) ~= t || t > n)
      error('Nelson:condest:TOutOfRange', 'T must be an integer between 1 and N = MAX(SIZE(A)).');
    end
  else
    t = min(n, 2);
  end

  matrixNorm = norm(A, 1);
  if (matrixNorm == 0)
    c = infinityLike(valueClass);
    v = ones(n, 1, valueClass) / n;
    varargout{1} = c;
    if (nargout > 1)
      varargout{2} = v;
    end
    return;
  end
  if (isClearlySingular(A))
    c = infinityLike(valueClass);
    v = ones(n, 1, valueClass) / n;
    varargout{1} = c;
    if (nargout > 1)
      varargout{2} = v;
    end
    return;
  end

  [inverseNorm, v, failed] = estimateInverseOneNorm(A, t, valueClass);
  if (failed)
    c = infinityLike(valueClass);
  else
    c = matrixNorm * inverseNorm;
  end

  varargout{1} = c;
  if (nargout > 1)
    varargout{2} = v;
  end
end
%=============================================================================
function tf = isClearlySingular(A)
  tf = false;
  n = size(A, 1);
  try
    if (sprank(A) < n)
      tf = true;
      return;
    end
  catch
  end
  if (n <= 128)
    try
      tf = rank(full(A)) < n;
    catch
      tf = false;
    end
  end
end
%=============================================================================
function [estimate, bestVector, failed] = estimateInverseOneNorm(A, t, valueClass)
  n = size(A, 1);
  maxIteration = 5;
  estimate = zeros(valueClass);
  bestVector = zeros(n, 1, valueClass);
  failed = false;

  for k = 1:t
    x = initialVector(n, k, valueClass);
    previousIndex = 0;
    for iter = 1:maxIteration
      [y, failed] = solveWithoutWarnings(A, x);
      if (failed)
        return;
      end
      yNorm = norm(y, 1);
      if (yNorm > estimate)
        estimate = yNorm;
        if (yNorm == 0)
          bestVector = zeros(n, 1, valueClass);
        else
          bestVector = y / yNorm;
        end
      end

      s = vectorSign(y);
      [z, failed] = solveWithoutWarnings(A', s);
      if (failed)
        return;
      end
      [~, currentIndex] = max(abs(z));
      if (currentIndex == previousIndex || abs(z(currentIndex)) <= real(z' * x))
        break;
      end
      x = zeros(n, 1, valueClass);
      x(currentIndex) = 1;
      previousIndex = currentIndex;
    end
  end
end
%=============================================================================
function x = initialVector(n, k, valueClass)
  if (k == 1)
    x = ones(n, 1, valueClass) / n;
  else
    x = zeros(n, 1, valueClass);
    x(1 + mod(k - 2, n)) = 1;
  end
end
%=============================================================================
function s = vectorSign(x)
  s = ones(size(x), class(x));
  nz = abs(x) ~= 0;
  s(nz) = x(nz) ./ abs(x(nz));
end
%=============================================================================
function [x, failed] = solveWithoutWarnings(A, b)
  previousWarning = warning();
  warning('off');
  failed = false;
  try
    x = A \ b;
  catch
    x = zeros(size(b));
    failed = true;
  end
  warning(previousWarning);
  if (~failed && any(~isfinite(x(:))))
    failed = true;
  end
end
%=============================================================================
function value = infinityLike(valueClass)
  value = zeros(valueClass) + Inf;
end
%=============================================================================
