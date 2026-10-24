%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = alphanumericsPattern(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  pat = pattern('[A-Za-z0-9]');
  if nargin == 0
    pat = asManyOfPattern(pat);
  elseif nargin == 1
    pat = pattern(['(?:', pattern.regexOf(pat), '){', int2str(varargin{1}), '}']);
  elseif nargin == 2
    pat = asManyOfPattern(pat, [varargin{1}, varargin{2}]);
  end
end
%=============================================================================
