%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = num2cell(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin == 1
    C = __num2cell__(varargin{1});
  else
    A = varargin{1};
    dims = varargin{2};
    sz = [size(A), ones(1, max(dims) - ndims(A))];
    rdims = 1:max(ndims(A), max(dims));
    rdims(dims) = []; 
    bsz(sort(dims)) = sz(dims);
    bsz(rdims) = 1;
    csz = sz;
    csz(dims) = 1;
    C = cell(csz);
    A = permute(A, [dims, rdims]); 
    offset = prod(bsz);
    ndx = 1:prod(bsz);
    for i = 0:prod(csz) - 1
      C{i + 1} = reshape(A(ndx + i * offset), bsz);
    end
  end
end
%=============================================================================
