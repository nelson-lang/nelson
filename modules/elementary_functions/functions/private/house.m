%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [houseVector, houseBeta, houseSigma] = house(inputVector, mode, className)
  numRows = size(inputVector, 1);
  numCols = size(inputVector, 2);
  if numCols > 1
    error('Nelson:validators:mustBeColumn', '%s must be a column vector.', 'Argument');
  end

  if nargin < 2
    activeMode = 0;
  else
    activeMode = normalizedMode(mode);
  end
  if nargin < 3 || isempty(className)
    outputClass = 'double';
  else
    outputClass = className;
  end

  houseVector = inputVector;
  tailNorm = norm(inputVector(2:numRows));
  totalNorm = norm([inputVector(1); tailNorm]);

  if totalNorm == 0
    houseBeta = ones(outputClass);
    houseSigma = zeros(outputClass);
    return
  end

  signedNorm = totalNorm * leadingSign(inputVector(1));
  activeMode = resolveMode(activeMode, inputVector, signedNorm);

  if activeMode == 0
    houseSigma = -signedNorm;
    houseVector(1) = houseVector(1) - houseSigma;
  else
    houseSigma = signedNorm;
    houseVector(1) = -tailNorm ^ 2 / (inputVector(1) + houseSigma)';
    if houseVector(1) == 0
      houseBeta = ones(outputClass);
      return
    end
  end
  houseBeta = -1 / (houseSigma' * houseVector(1));
end
%=============================================================================
function activeMode = normalizedMode(mode)
  if isempty(mode) || ~isnumeric(mode) || ~isscalar(mode)
    activeMode = 0;
  else
    activeMode = mode;
  end
end
%=============================================================================
function signValue = leadingSign(firstValue)
  signValue = sign(firstValue);
  if signValue == 0
    signValue = 1;
  end
end
%=============================================================================
function activeMode = resolveMode(activeMode, inputVector, signedNorm)
  if activeMode ~= 2
    return
  end
  if any(imag(inputVector))
    activeMode = 0;
  elseif signedNorm < 0
    activeMode = 0;
  else
    activeMode = 1;
  end
end
%=============================================================================
