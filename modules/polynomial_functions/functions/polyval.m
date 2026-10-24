%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, delta] = polyval(p, x, S, mu)
  narginchk(2, 4);
  nargoutchk(0, 2);
  if nargin >= 4 && ~isempty(mu)
    evaluationPoints = (x - mu(1)) ./ mu(2);
  else
    evaluationPoints = x;
  end

  coefficientCount = numel(p);
  y = hornerEvaluate(p, evaluationPoints, coefficientCount);
  if nargout < 2
    return
  end
  if nargin < 3 || isempty(S)
    error('Nelson:polyval:missingS', 'S is required to compute error estimates.');
  end
  delta = polynomialDelta(S, evaluationPoints, coefficientCount);
end
%=============================================================================
function y = hornerEvaluate(coefficients, evaluationPoints, coefficientCount)
  if isempty(coefficients)
    y = zeros(size(evaluationPoints));
    return
  end
  % The fused C++ kernel evaluates real and complex floating-point data in a
  % single pass and is bit-identical to the loop below. Sparse data stays on the
  % loop (the kernel reads a dense buffer).
  if isfloat(coefficients) && isfloat(evaluationPoints) ...
      && ~issparse(coefficients) && ~issparse(evaluationPoints)
    y = polyval_horner(coefficients, evaluationPoints);
    return
  end
  y = coefficients(1) + zeros(size(evaluationPoints));
  for coefficientIndex = 2:coefficientCount
    y = y .* evaluationPoints + coefficients(coefficientIndex);
  end
end
%=============================================================================
function delta = polynomialDelta(S, evaluationPoints, coefficientCount)
  xColumn = evaluationPoints(:);
  vandermondeLike = ones(numel(xColumn), coefficientCount) + 0 * xColumn;
  for coefficientIndex = coefficientCount - 1:-1:1
    vandermondeLike(:, coefficientIndex) = ...
      xColumn .* vandermondeLike(:, coefficientIndex + 1);
  end
  normalizedDesign = vandermondeLike / S.R;
  leverage = sqrt(1 + sum(normalizedDesign .* conj(normalizedDesign), 2));
  if S.df == 0
    warning('Nelson:polyval:zeroDegreesOfFreedom', 'Zero degrees of freedom; delta is set to Inf.');
    delta = Inf(size(evaluationPoints));
  else
    delta = (S.normr / sqrt(S.df)) * leverage;
    delta = reshape(delta, size(evaluationPoints));
  end
end
%=============================================================================
