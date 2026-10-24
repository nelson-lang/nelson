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
function varargout = d2c(varargin)
  % [a, b] = d2c(phi, gamma, t)

  narginchk(3, 3);
  nargoutchk(0, 2);

  phi = varargin{1};
  gamma = varargin{2};
  T = varargin{3};

  [msg, phi, gamma] = abcdchk(phi, gamma); %#ok<NLS0004>
  if ~isempty(msg)
    error('Nelson:control_system:invalidStateSpace', msg);
  end

  rowCount = size(phi, 1);
  columnCount = size(phi, 2);
  inputCount = size(gamma, 2);

  if rowCount == 1 && phi == 1
    a = 0;
    b = gamma / T;
    return
  end

  b = zeros(rowCount, inputCount);
  nonzero = find(any(gamma ~= 0, 1));
  nonzeroCount = length(nonzero);

  augmentedMatrix = [[phi, gamma(:, nonzero)]; zeros(nonzeroCount, columnCount), eye(nonzeroCount)];
  [logMatrix, ~] = logm(augmentedMatrix); %#ok<NLS0004>
  logMatrix = real(logMatrix / T);
  a = logMatrix(1:columnCount, 1:columnCount);
  if length(b)
    b(:, nonzero) = logMatrix(1:columnCount, columnCount + 1:columnCount + nonzeroCount);
  end
end
