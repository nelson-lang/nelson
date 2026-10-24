%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pp = mkpp(breaks, coefs, d)
  narginchk(2, 3);
  nargoutchk(0, 1);
  if nargin < 3
    d = 1;
  end
  if ~isvector(breaks) || numel(breaks) < 2
    error('Nelson:mkpp:invalidBreaks', 'BREAKS must be a vector with at least two elements.');
  end
  breaks = full(breaks(:)).';
  pieces = numel(breaks) - 1;
  dprod = prod(d);
  order = size(coefs, 2);
  if ndims(coefs) > 2
    error('Nelson:mkpp:invalidCoefs', 'COEFS must be a matrix.');
  end
  if size(coefs, 1) ~= pieces * dprod
    error('Nelson:mkpp:invalidCoefsSize', 'The number of rows of COEFS must equal PIECES times PROD(D).');
  end
  pp = struct('form', 'pp', 'breaks', breaks, 'coefs', coefs, ...
    'pieces', pieces, 'order', order, 'dim', d);
end
%=============================================================================
