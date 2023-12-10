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
function varargout = tf2ss(numerator, denominator)
  narginchk(2, 2);
  nargoutchk(0, 4);
  
  % Ensure the denominator is a row vector
  denominator = denominator(:)';
  if issingle(numerator) || issingle(denominator)
    numerator = single(numerator);
    denominator = single(denominator);
  end
  if isempty(numerator) && isempty(denominator)
    A = zeros(0, 'like', numerator);
    B = zeros(0, 'like', numerator);
    C = zeros(0, 'like', numerator);
    D = zeros(0, 'like', numerator);
  else 
    % Normalize transfer function to have a leading coefficient of 1
    numerator = numerator / denominator(1);
    denominator = denominator / denominator(1);
    
    % Ensure lengths of numerator and denominator match
    numerator = padNumeratorWithZeros(numerator, denominator);
    
    % Remove leading zero columns
    denominator = removeLeadingZeroColumns(denominator);
    numerator = removeLeadingZeroColumns(numerator);
    
    % Remove leading zeros from the denominator
    idx = find(denominator);
    if ~isempty(idx)
      denominator = denominator(idx(1):end);
    end
  
    [mb, nb] = size(numerator);
    na = size(denominator, 2);
  
    % Pad numerator with zeros if necessary
    if nb < na
      numerator = padNumeratorWithZeros(numerator, na);
    end
  
    [A, B, C, D] = computeStateSpaceMatrices(numerator, denominator, mb, nb, na);
  end

  % Output state-space matrices
  varargout{1} = A;
  if nargout > 1
    varargout{2} = B;
  end
  if nargout > 2
    varargout{3} = C;
  end
  if nargout > 3
    varargout{4} = D;
  end
end
%=============================================================================
function numerator = padNumeratorWithZeros(numerator, targetLength)
  if length(numerator) < targetLength
    % Pad numerator with zeros
    numerator = [zeros(1, targetLength - length(numerator)), numerator];
  end
end
%=============================================================================
function [A, B, C, D] = computeStateSpaceMatrices(numerator, denominator, mb, nb, na)
  if na == 1
    A = [];
    B = zeros(0, 1);
    C = zeros(mb, 0);
    D = numerator / denominator;
  else
    numerator = numerator / denominator(1);
    denominator = denominator(2:na) / denominator(1);
    A = [-denominator; eye(na - 2, na - 1)];
    B = eye(na - 1, 1);
    C = numerator(:, 2:nb) - diag(numerator(:, 1)) * repmat(denominator, mb, 1);
    D = numerator(:, 1);
  end
end
%=============================================================================
function p = removeLeadingZeroColumns(p)
  [n, m] = size(p);
  % Check if there are multiple columns and the norm of the first column is zero
  if m > 1 && norm(p(:, 1)) == 0
    % Remove the first column and recursively call removeLeadingZeroColumns
    p(:, 1) = [];
    p = removeLeadingZeroColumns(p);
  end
end
%=============================================================================
