%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function counts = countcats(A, varargin)
  narginchk(1, 2);
  nCategories = length(A.categoryNames);
  codes = A.codes;
  if nargin == 1
    if isrow(codes)
      counts = __categorical_codes_count__(codes, nCategories).';
      return
    end
    if iscolumn(codes)
      counts = __categorical_codes_count__(codes, nCategories);
      return
    end
    dim = find(size(codes) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  else
    dim = varargin{1};
  end
  if dim == 1
    counts = zeros(nCategories, size(codes, 2));
    for col = 1:size(codes, 2)
      counts(:, col) = __categorical_codes_count__(codes(:, col), nCategories);
    end
  elseif dim == 2
    counts = zeros(size(codes, 1), nCategories);
    for row = 1:size(codes, 1)
      counts(row, :) = __categorical_codes_count__(codes(row, :), nCategories).';
    end
  else
    error('Nelson:countcats:dimensionNotSupported', 'Dimension argument is not supported for categorical countcats.');
  end
end
%=============================================================================
