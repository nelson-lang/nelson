%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [a, b] = polyder(varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  numerator = polynomialRow(varargin{1});
  if nargin < 2
    denominator = 1;
  else
    denominator = polynomialRow(varargin{2});
  end
  numeratorDerivative = derivativeCoefficients(numerator);
  denominatorDerivative = derivativeCoefficients(denominator);
  leftTerm = conv(numeratorDerivative, denominator);
  rightTerm = conv(numerator, denominatorDerivative);
  padding = length(leftTerm) - length(rightTerm);
  if padding > 0
    rightTerm = [zeros(1, padding), rightTerm];
  elseif padding < 0
    leftTerm = [zeros(1, -padding), leftTerm];
  end
  if nargout < 2
    a = trimDerivative(leftTerm + rightTerm);
  else
    a = trimDerivative(leftTerm - rightTerm);
  end
  b = trimDenominator(conv(denominator, denominator), denominator);
  maximumOrder = max(length(numerator) + length(denominator) - 2, 1);
  if length(a) > maximumOrder
    a = a(2:end);
  end
end
%=============================================================================
function p = polynomialRow(p)
  p = p(:).';
end
%=============================================================================
function dp = derivativeCoefficients(p)
  degree = length(p) - 1;
  if degree > 0
    dp = p(1:degree) .* (degree:-1:1);
  else
    dp = 0;
  end
end
%=============================================================================
function p = trimDerivative(p)
  first = find(p ~= 0, 1);
  if isempty(first)
    p = 0;
  else
    p = p(first:end);
  end
end
%=============================================================================
function p = trimDenominator(p, source)
  first = find(p ~= 0, 1);
  if isempty(first)
    p = zeros(class(source));
  else
    p = p(first:end);
  end
end
%=============================================================================
