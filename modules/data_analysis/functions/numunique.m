%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = numunique(A, varargin)
  % number of distinct values (or rows with 'rows'); every NaN counts on its own
  narginchk(1, 2);
  if nargin == 2
    option = varargin{1};
    if ~((ischar(option) || isstring(option)) && strcmpi(char(option), 'rows'))
      error('Nelson:numunique:invalidOption', 'The only supported option is ''rows''.');
    end
    n = size(unique(A, 'rows'), 1);
  else
    n = numel(unique(A));
  end
  n = double(n);
end
%=============================================================================
