%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = whitespacePattern(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  base = pattern('[\s\x{00A0}\x{2000}-\x{200A}]');
  if nargin == 0
    % With no count, whitespacePattern matches one or more whitespace
    % characters (a maximal run), matching expected behavior.
    pat = pattern([pattern.regexOf(base), '+']);
  elseif nargin == 1
    pat = pattern(['(?:', pattern.regexOf(base), '){', int2str(varargin{1}), '}']);
  else
    pat = asManyOfPattern(base, [varargin{1}, varargin{2}]);
  end
end
%=============================================================================
