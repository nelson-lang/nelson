%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = maxk (varargin)
  narginchk(2, 3);
  nargoutchk(0, 2);

  A = varargin{1};
  k = varargin{2};

  % Determine dimension argument if provided, otherwise compute default
  if nargin >= 3
    dim = varargin{3};
  else
    if min (size (A)) != 1
      % For matrices (both dims > 1) keep original behavior: work along columns
      dim = 1;
    else
      % For vectors use the vector's non-singleton dimension
      dim = find (size (A) != 1, 1);
      if isempty (dim)
        dim = 1;
      end
    end
  end

  sz = size (A);
  if k > sz(dim)
    error (_('Number of elements exceeds size of array.'));
  end

  % Sort along requested dimension in descending order and take first k entries
  [s, ind] = sort (A, dim, 'descend');

  subs = repmat ({':'}, 1, ndims (A));
  subs{dim} = 1:k;
  val = s(subs{:});
  idx = ind(subs{:});

  % Assign outputs via varargout
  varargout{1} = val;
  if nargout > 1
    varargout{2} = idx;
  end
end
%=============================================================================
