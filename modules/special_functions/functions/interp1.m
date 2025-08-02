%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = interp1(varargin)
  % vq = interp1(v, xq)
  % vq = interp1(v, xq, 'linear')
  % vq = interp1(x, v, xq)
  % vq = interp1(x, v, xq, 'linear')
  narginchk(2, 4);
  nargoutchk(0, 1);
  switch (nargin)
    case 2
      v = varargin{1};
      xq = varargin{2};
      vq = linearInterpolation1DVXQ(v, xq);
    case 3
      xqOrOption = varargin{3};
      isOption = false;
      if ischar(xqOrOption) || isStringScalar(xqOrOption)
        option = convertStringsToChars(xqOrOption);
        if ~strcmp(option, 'linear')
          error(_('linear method expected.'));
        end
        isOption = true;
      end
      if isOption
        v = varargin{1};
        xq = varargin{2};
        vq = linearInterpolation1DVXQ(v, xq);
      else
        x = varargin{1};
        v = varargin{2};
        xq = varargin{3};
        vq = linearInterpolation1DXVXQ(x, v, xq);
      end
    case 4
      x = varargin{1};
      v = varargin{2};
      xq = varargin{3};
      option = varargin{4};
      if ischar(option) || isStringScalar(option)
        option = convertStringsToChars(option);
        if ~strcmp(option, 'linear')
          error(_('linear method expected.'));
        end
      end
      vq = linearInterpolation1DXVXQ(x, v, xq);
    end
    varargout{1} = vq;
end
%=============================================================================
function vq = linearInterpolation1DXVXQ(x, v, xq)
  vq = [];
  if isvector(v)
    vq = __interp1__(x, v, xq);
  else
    for k = 1:size(v, 2)
      vq(:, k) = __interp1__(x, v(:, k), xq);
    end
  end
end
%=============================================================================
function vq = linearInterpolation1DVXQ(v, xq)
  vq = [];
  if isvector(v)
    vq = __interp1__(v, xq);
  else
    for k = 1:size(v, 2)
      vq(:, k) = __interp1__(v(:, k), xq);
    end
  end
end
%=============================================================================
