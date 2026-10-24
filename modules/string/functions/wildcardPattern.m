%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = wildcardPattern(varargin)
  narginchk(0, 4);
  nargoutchk(0, 1);
  expr = '.';
  if nargin >= 2 && strcmpi(char(varargin{1}), 'Except')
    expr = ['(?:(?!', pattern.regexOf(varargin{2}), ').)'];
    pat = asFewOfPattern(pattern(expr));
    return
  elseif nargin >= 4 && strcmpi(char(varargin{3}), 'Except')
    expr = ['(?:(?!', pattern.regexOf(varargin{4}), ').)'];
  end
  pat = pattern(expr);
  if nargin == 0
    pat = asFewOfPattern(pat);
  elseif nargin == 1
    pat = asFewOfPattern(pat, varargin{1});
  elseif nargin >= 2
    pat = asFewOfPattern(pat, [varargin{1}, varargin{2}]);
  end
end
%=============================================================================
