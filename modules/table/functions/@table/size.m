%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = size(varargin)
  % s = size(X)
  % sdim = size(X, dim)
  % vec = size(X, dims)
  % [r, c] = size(X)
  % [s1, ... , sn] = size(X)
  
  narginchk(1, 100);
  
  % Get dimensions of input
  T = struct(varargin{1});
  names = fieldnames(T.data);
  width = length(names);
  if ~isempty(names)
    height = size(T.data.(names{1}), 1);
  else
    height = 0;
  end
  
  % Handle different input cases
  if nargin == 1
    varargout = handleSingleInput(height, width, nargout);
  elseif nargin == 2
    varargout = handleTwoInputs(height, width, varargin{2});
  else
    varargout = handleMultipleInputs(height, width, varargin(2:end));
  end
end
%=============================================================================
function out = handleSingleInput(height, width, nargout)
  if nargout < 2
    out = {[height, width]};
  elseif nargout == 2
    out = {height, width};
  else
    out = [{height, width}, num2cell(ones(1, nargout-2))];
  end
end
%=============================================================================
function out = handleTwoInputs(height, width, dim)
  if isscalar(dim) && isnumeric(dim)
    nargoutchk(0, 1);
    out = {getDimSize(height, width, dim)};
  elseif isvector(dim) && isnumeric(dim) && ~isempty(dim)
    out = getMultipleDimSizes(height, width, dim);
  else
    error('Wrong dimension.');
  end
end
%=============================================================================
function out = handleMultipleInputs(height, width, dims)
  dims = cell2mat(dims);
  if (~isnumeric(dims) || numel(dims) ~= numel(dims))
    error('Wrong dimension.');
  end
  out = getMultipleDimSizes(height, width, dims);
end
%=============================================================================
function size = getDimSize(height, width, dim)
  switch dim
    case 1
      size = height;
    case 2
      size = width;
    otherwise
      size = 1;
    end
  end
  %=============================================================================
function out = getMultipleDimSizes(height, width, dims)
  out = ones(1, numel(dims));
  out(dims == 1) = height;
  out(dims == 2) = width;
  if nargout < 2
    out = {out};
  else
    out = num2cell(out);
  end
end
%=============================================================================
