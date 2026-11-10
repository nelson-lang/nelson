%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Z = pagemtimes(varargin)
  % one device product per page; option forms and general page broadcasting run on the host
  if nargin == 2 && isnumeric(varargin{1}) && isnumeric(varargin{2}) ...
      && isreal(varargin{1}) && isreal(varargin{2})
    X = varargin{1};
    Y = varargin{2};
    sx = size(X);
    sy = size(Y);
    count = max(numel(sx), numel(sy));
    sx(end + 1:count) = 1;
    sy(end + 1:count) = 1;
    px = prod(sx(3:end));
    py = prod(sy(3:end));
    if sx(2) == sy(1) && (isequal(sx(3:end), sy(3:end)) || px == 1 || py == 1)
      if px >= py
        pageDims = sx(3:end);
      else
        pageDims = sy(3:end);
      end
      pages = max(px, py);
      Z = zeros([sx(1), sy(2), pages], 'like', gpuArray(single(0)));
      for k = 1:pages
        Z(:, :, k) = X(:, :, min(k, px)) * Y(:, :, min(k, py));
      end
      Z = reshape(Z, [sx(1), sy(2), pageDims]);
      return
    end
  end
  Z = __gpuArray_host__('pagemtimes', varargin{:});
end
%=============================================================================
