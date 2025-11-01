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
  [numRows, numCols] = size(inputVector);
  if numCols > 1
    error(_('Argument must be a column vector.'));
  end
  
  % robust default for mode
  if nargin < 2 || isempty(mode) || ~isnumeric(mode) || ~isscalar(mode)
    mode = 0;
  end
  if nargin < 3 || isempty(className)
    className = 'double';
  end
  
  houseVector = inputVector;
  tailNorm = norm(inputVector(2:numRows));
  totalNorm = norm([inputVector(1); tailNorm]);
  
  if totalNorm == 0
    houseBeta = ones(className);
    houseSigma = zeros(className);
    return
  end
  
  signVal = sign(inputVector(1));
  if signVal == 0
    signVal = 1;
  end
  s_pre = totalNorm * signVal;
  
  if mode == 2
    if ~any(imag(inputVector))
      if s_pre < 0
        mode = 0;
      else
        mode = 1;
      end
    else
      mode = 0;
    end
  end
  
  if mode == 0
    houseSigma = -s_pre;
    houseVector(1) = houseVector(1) - houseSigma;
  else
    houseSigma = s_pre;
    houseVector(1) = -tailNorm^2 / (inputVector(1) + houseSigma)';
    if houseVector(1) == 0
      houseBeta = ones(className);
      return
    end
  end
  houseBeta = -1/(houseSigma'*houseVector(1));
end
%=============================================================================
