%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bernsteinMatrix(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  approximationOrder = varargin{1};
  evaluationPoint = varargin{2};
  
  validateInputs(approximationOrder, evaluationPoint)
  
  if isempty(evaluationPoint) 
    varargout{1} = ones(0, approximationOrder + 1);
    return
  end
  
  [B, T, TT] = calculateCoefficients(approximationOrder, evaluationPoint);
  
  varargout{1} = computeBernsteinMatrix(T, TT);
end    
%=============================================================================
function validateInputs(approximationOrder, evaluationPoint)
  mustBeScalarOrEmpty(approximationOrder, 1);
  mustBeNonempty(approximationOrder, 1);
  mustBeReal(approximationOrder, 1);
  mustBePositive(approximationOrder, 1);
  mustBeInteger(approximationOrder, 1);
  
  if ~isempty(evaluationPoint)
    mustBeVector(evaluationPoint, 2);
  end
end
%=============================================================================
function [B, T, TT] = calculateCoefficients(approximationOrder, evaluationPoint)
  B = calculateBinomialCoefficients(approximationOrder);
  
  if ~isempty(evaluationPoint)
    T = calculateTMatrix(approximationOrder, evaluationPoint, B);
    TT = calculateTTMatrix(approximationOrder, evaluationPoint);
  else
    T = [];
    TT = [];
  end
end
%=============================================================================
function B = calculateBinomialCoefficients(approximationOrder)
  B = ones(approximationOrder + 1, 1); 
  for j = 1:ceil(approximationOrder / 2)
    B(j+1) = B(j) * (approximationOrder + 1 - j) / j;
    B(approximationOrder + 1 - j) = B(j + 1);
  end
end
%=============================================================================
function T = calculateTMatrix(approximationOrder, evaluationPoint, B)
  T = ones(length(evaluationPoint), approximationOrder + 1);
  evaluationPoint = reshape(evaluationPoint, length(evaluationPoint), 1);
  for j = 1:approximationOrder 
    T(:, j + 1) = evaluationPoint .* T(:, j); 
    T(:, j) = T(:, j) .* B(j);
  end
end
%=============================================================================
function TT = calculateTTMatrix(approximationOrder, evaluationPoint)
  TT = ones(length(evaluationPoint), approximationOrder + 1);
  evaluationPoint = reshape(evaluationPoint, length(evaluationPoint), 1);
  tt = 1 - evaluationPoint;
  for j = 1:approximationOrder 
    TT(:, approximationOrder + 1 - j) = tt .* TT(:, approximationOrder + 2 - j);
  end
end
%=============================================================================
function result = computeBernsteinMatrix(T, TT)
  result = T .* TT;
end
%=============================================================================
