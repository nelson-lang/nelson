%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = cat(varargin)
  % a builtin will be faster but currently it does the job :)
  if (nargin < 1)
    error('Nelson:minrhs', _('Not enough input arguments.'));
  end
  if (nargin == 1)
    dim = varargin{1};
    if isscalar(dim) && (dim > 0)
      result = [];
      return
    else
      error('Nelson:catenate:invalidDimension', _('Dimension argument must be a real, positive, integer scalar.'));
    end
  end
  if nargin == 2
    result = varargin{2};
    return
  else
    dim = varargin{1};
    if (~isscalar(dim) || dim < 1)
      error('Nelson:catenate:invalidDimension', _('Dimension argument must be a real, positive, integer scalar.'));
    elseif (dim == 1)
      result = vertcat(varargin{2:end});
    elseif (dim == 2)
      result = horzcat(varargin{2:end});
    else
      for jj = 2:nargin
        if ~isempty(varargin{jj})
          break
        end
      end
      result = varargin{jj};
      if isempty(result)
        return
      end
      for ii = jj+1:nargin
        if isempty(varargin{ii})
          continue
        end
        result = dim_concat(varargin{1}, result, varargin{ii});
      end
    end
  end
end
%=============================================================================
function C = dim_concat(dim, A, B) 
  ndimA = numel(size(A));
  ndimB = numel(size(B));
  ndimC = max([dim, ndimA, ndimB]);
  sizA2 = ones(1, ndimC);
  sizB2 = sizA2;
  sizA2(1:ndimA) = size(A);
  sizB2(1:ndimB) = size(B);
  
  if (sizA2([1:dim-1, dim+1:ndimC]) == sizB2([1:dim-1, dim+1:ndimC]))
    sizC = sizA2;
    sizC(dim) = sizA2(dim) + sizB2(dim);
    C = zeros(sizC);
    dA = cell(ndimC, 1);
    for ii = 1:ndimC
      dA{ii} = 1:sizA2(ii);
    end
    dB = dA;
    dB{dim} = 1:sizB2(dim);
    dAC = dA;
    dBC = dB;
    dBC{dim} = sizA2(dim) + dBC{dim};
    C(dAC{:})  = A(dA{:});
    C(dBC{:})  = B(dB{:});
  else
    error('Nelson:catenate:dimensionMismatch', _('Dimensions of arrays being concatenated are not consistent.'));
  end
end
%=============================================================================
