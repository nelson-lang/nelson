%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xcov(varargin)
  narginchk(1, 4);
  nargoutchk(0, 2);

  xInput = varargin{1};
  if isvector(xInput)
    x = xInput(:) - mean(xInput(:));
  else
    x = xInput - mean(xInput, 1);
  end
  args = varargin(2:end);
  if ~isempty(args) && ~(ischar(args{1}) || isstring(args{1})) && ~isscalar(args{1})
    y = args{1};
    args{1} = y(:) - mean(y(:));
    [c, lags] = xcorr(x, args{:});
  else
    [c, lags] = xcorr(x, args{:});
  end

  if size(xInput, 1) == 1
    c = c.';
    lags = lags.';
  end

  varargout{1} = c;
  if nargout > 1
    varargout{2} = lags;
  end
end
%=============================================================================
