%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = gradient (varargin)
% gradient: Compute gradient along specified dimensions.
%   Computes the gradient of the input data along the specified dimensions.
% FX = gradient(F)
% [FX, FY] = gradient(F)
% [FX, FY, FZ, ... , FN] = gradient(F)
% [...] = gradient(F, h)
% [...] = gradient(F, hx, hy, ... , hN)

  m = varargin{1};
  inputArguments= varargin(2:end);
  
  if (isvector(m))
    needTranspose = (size(m, 2) == 1);
    m = m(:).';
  else 
    needTranspose = false;
  end
  
  [d, nd] = computeD(m, inputArguments, nargin);
  m = shiftdim(m, 1);
  for i = 1:min(nd, nargout)
    [varargout{i}, m] = computeGradient(m, d, i, nd);
  end
  
  if (needTranspose)
    varargout{1} = varargout{1}.';
  end
end
%=============================================================================
function [d, nd] = computeD(m, inputArguments, nbInputs)
% computeD: Compute differences for gradient computation.
%   Computes the differences required for gradient computation along each dimension.
%
%   Input arguments:
%       - m: Input data.
%       - inputArguments: Additional arguments for gradient computation.
%       - nbInputs: Number of input arguments provided.
%
%   Output arguments:
%       - d: Cell array containing differences along each dimension.
%       - nd: Number of dimensions in the input data.    
  nd = ndims (m);
  sz = size (m);
  if (length (sz) > 1)
    tmp = sz(1);
    sz(1) = sz(2);
    sz(2) = tmp;
  end
  
  d = cell (1, nd);
  if (nbInputs == 1)
    for i = 1:nd
      d{i} = ones (sz(i) - 1, 1);
    end
  elseif (nbInputs == 2)
    if (isscalar (inputArguments{1}))
      for i = 1:nd
        d{i} = inputArguments{1} * ones (sz(i) - 1, 1);
      end
    else
      d{1} = diff (inputArguments{1}(:));
    end
  else
    if (nbInputs < nd)
      error(_('The number of spacing values do not match.'));
    end
    for i = 1:nd
      if (isscalar(inputArguments{i}))
        d{i} = inputArguments{i} * ones (sz(i) - 1, 1);
      else
        d{i} = diff (inputArguments{i}(:));
      end
    end
  end
end
%=============================================================================
function [result, m] = computeGradient(m, d, idx, nd)
% computeGradient: Compute gradient along a specific dimension.
%   Computes the gradient of the input data along the specified dimension.
%
%   Input arguments:
%       - m: Input data.
%       - d: Cell array containing differences along each dimension.
%       - idx: Index of the dimension for gradient computation.
%       - nd: Number of dimensions in the input data.
%
%   Output arguments:
%       - result: Gradient of the input data along the specified dimension.
%       - m: Updated input data after gradient computation.
  m_rows = size(m, 1);
  mc = numel (m) / m_rows;
  Y = zeros (size (m), class (m));
  
  if (m_rows > 1)
    Y(1,:) = diff(m(1:2, :)) / d{idx}(1);
    Y(m_rows,:) = diff (m(m_rows-1:m_rows, :) / d{idx}(m_rows - 1));
  end
  
  if (m_rows > 2)
    Y(2:m_rows-1, :) = ((m(3:m_rows, :) - m(1:m_rows-2, :)) ./ kron(d{idx}(1:m_rows-2) + d{idx}(2:m_rows-1), ones(1, mc)));
  end
  
  if (idx == 1)
    result = shiftdim(Y, nd - 1);
    m = shiftdim(m, nd - 1);
  elseif (idx == 2)
    result = Y;
    m = shiftdim(m, 2);
  else
    result = shiftdim(Y, nd - idx + 1);
    m = shiftdim(m, 1);
  end
end
%=============================================================================