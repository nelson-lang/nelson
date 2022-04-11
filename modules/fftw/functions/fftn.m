%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = fftn(varargin)
  % N-D fast Fourier transform
  % Y = fftn(X)
  % Y = fftn(X, dims)

  if ~(nargin() == 1 || nargin() == 2)
    error(_('Wrong number of input arguments.'));
  end
  X = varargin{1};
  if nargin() == 2
    dims = varargin{2};
  else
    dims = size(X);
  end
  dims = dims(:)';
  if (numel(dims) < ndims(X))
    points = size(X);
    dims = [dims, points((numel(dims) + 1):end)];
  end
  Y = X;
  if ~issingle(Y)
    Y = double(Y);
   end
   if (numel(Y) ~= 0)
     for n=1:numel(dims)
       Y = fft(Y, dims(n), n);
     end
    end
end
%=============================================================================
