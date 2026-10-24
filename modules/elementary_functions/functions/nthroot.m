%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nthroot(varargin)
  % Check that there are exactly 2 input arguments
  narginchk(2, 2);

  % Check that there is at most 1 output argument
  nargoutchk(0, 1);

  % Extract the input arguments
  numberX = varargin{1};
  rootN = varargin{2};
  if islogical(numberX) || ischar(numberX)
    numberX = double(numberX);
  end
  if islogical(rootN) || ischar(rootN)
    rootN = double(rootN);
  end

  % Validate the input arguments
  validateInputs(numberX, rootN);

  % Compute the Nth root and assign it to the output argument
  varargout{1} = computeNthRoot(numberX, rootN);
end
%=============================================================================
function validateInputs(numberX, rootN)
  % Check if both inputs are real numbers
  if ~isreal(numberX) || ~isreal(rootN)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'X and N');
  end

  % Check if the inputs have compatible sizes (implicit expansion)
  if ~isscalar(numberX) && ~isscalar(rootN) && ~isequal(size(numberX), size(rootN))
    sizeX = size(numberX);
    sizeN = size(rootN);
    maxDims = max(numel(sizeX), numel(sizeN));
    sizeX(end + 1:maxDims) = 1;
    sizeN(end + 1:maxDims) = 1;
    if any(sizeX ~= sizeN & sizeX ~= 1 & sizeN ~= 1)
      error('Nelson:elementary_functions:xNEitherScalarsArrays', 'X and N must either be scalars or arrays of the same size.');
    end
  end

  % Check if negative values of X have valid corresponding values of N
  if any(numberX < 0 & mod(rootN, 2) ~= 1, 'all')
    error('Nelson:elementary_functions:ifXNegativeNOdd', 'If X is negative, then N must be an odd integer.');
  end
end
%=============================================================================
function result = computeNthRoot(numberX, rootN)
  % Compute the initial approximation of the Nth root
  result = (sign(numberX) + (numberX == 0)) .* (abs(numberX) .^ (1 ./ rootN));

  % Identify elements that require iterative correction for accuracy
  requiresCorrection = numberX ~= 0 & (abs(numberX) < 1 ./ eps(class(result))) & isfinite(rootN);

  % Apply iterative correction where necessary
  if any(requiresCorrection, 'all')
    % Apply Newton's method for better accuracy
    correctedResult = result - (result .^ rootN - numberX) ./ (rootN .* result .^ (rootN - 1));

    % Update the result where correction is needed
    if all(requiresCorrection, 'all')
      result = correctedResult;
    else
      result(requiresCorrection) = correctedResult(requiresCorrection);
    end
  end
end
%=============================================================================
