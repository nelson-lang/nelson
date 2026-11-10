%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = convn(A, B, shape)
  % N-D convolution of A and B with the 'full', 'same' or 'valid' shape
  narginchk(2, 3);
  if nargin < 3
    shape = 'full';
  end
  if ~((ischar(shape) || isstring(shape)) && any(strcmpi(char(shape), {'full', 'same', 'valid'})))
    error('Nelson:convn:invalidShape', 'Shape must be ''full'', ''same'' or ''valid''.');
  end
  shape = lower(char(shape));
  if ~(isnumeric(A) || islogical(A) || ischar(A)) || ~(isnumeric(B) || islogical(B) || ischar(B))
    error('Nelson:convn:invalidType', 'Inputs must be numeric, char or logical arrays.');
  end
  if ~isfloat(A)
    A = double(A);
  end
  if ~isfloat(B)
    B = double(B);
  end
  if ndims(A) <= 2 && ndims(B) <= 2
    C = conv2(A, B, shape);
    return
  end
  sa = size(A);
  sb = size(B);
  nd = max(numel(sa), numel(sb));
  sa(end + 1:nd) = 1;
  sb(end + 1:nd) = 1;
  fullSize = max(sa + sb - 1, 0);
  proto = zeros(1, 'like', A) + zeros(1, 'like', B);
  C = zeros(fullSize, 'like', proto);
  if all(fullSize > 0) && numel(A) > 0 && numel(B) > 0
    if numel(B) <= numel(A)
      K = B;
      G = A;
    else
      K = A;
      G = B;
    end
    sk = size(K);
    sk(end + 1:nd) = 1;
    sg = size(G);
    sg(end + 1:nd) = 1;
    subs = cell(1, nd);
    [subs{:}] = ind2sub(sk, (1:numel(K)).');
    span = cell(1, nd);
    for k = 1:numel(K)
      for d = 1:nd
        span{d} = subs{d}(k) + (0:sg(d) - 1);
      end
      C(span{:}) = C(span{:}) + K(k) * G;
    end
  end
  switch shape
    case 'same'
      first = floor(sb / 2) + 1;
      count = sa;
    case 'valid'
      first = sb;
      count = max(sa - sb + 1, 0);
    otherwise
      return
  end
  span = cell(1, nd);
  for d = 1:nd
    span{d} = first(d) + (0:count(d) - 1);
  end
  C = C(span{:});
end
%=============================================================================
