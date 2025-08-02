%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function l = logspace(varargin)
  if (nargin ~= 2 && nargin ~= 3)
    error(_('Wrong number of input arguments.'));
  end
  if nargin == 2
    n = 50;
  else
    n = varargin{3};
  end
  s = varargin{1};
  e = varargin{2};
  if e == single(pi) || e == pi
    e = log10(e);
  end
  l = 10 .^ linspace(s, e, n);
end
%=============================================================================
