%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = maxk(varargin)
  narginchk(2, 3);
  nargoutchk(0, 2);

  A = varargin{1};
  k = varargin{2};

  % Determine dimension argument if provided, otherwise compute default
  if nargin >= 3
    dim = varargin{3};
  else
    if min(size(A)) != 1
      % For matrices (both dims > 1) keep original behavior: work along columns
      dim = 1;
    else
      % For vectors use the vector's non-singleton dimension
      dim = find(size(A) != 1, 1);
      if isempty(dim)
        dim = 1;
      end
    end
  end

  sz = size(A);
  if k > sz(dim)
    % k larger than the dimension: return all elements sorted (reference-implementation semantics)
    k = sz(dim);
  end

  % Sort along requested dimension in descending order and take first k entries
  % NaN values must be considered smaller than any other value (placed last),
  % so sort on keys where NaN is replaced by -Inf, then gather values from A.
  subs = repmat({':'}, 1, ndims(A));
  subs{dim} = 1:k;
  if isfloat(A) && any(isnan(A(:)))
    order = [dim, 1:dim - 1, dim + 1:ndims(A)];
    Aperm = permute(A, order);
    szPerm = size(Aperm);
    A2 = reshape(Aperm, szPerm(1), []);
    Akey = A2;
    Akey(isnan(Akey)) = -Inf;
    [~, ind2] = sort(Akey, 1, 'descend');
    idx2 = ind2(1:k, :);
    cols = repmat(1:size(A2, 2), k, 1);
    val2 = A2(idx2 + (cols - 1) * size(A2, 1));
    szPerm(1) = k;
    val = ipermute(reshape(val2, szPerm), order);
    idx = ipermute(reshape(idx2, szPerm), order);
  else
    [s, ind] = sort(A, dim, 'descend');
    val = s(subs{:});
    idx = ind(subs{:});
  end

  % Assign outputs via varargout
  varargout{1} = val;
  if nargout > 1
    varargout{2} = idx;
  end
end
%=============================================================================
