%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = asFewOfPattern(basePattern, count, maxCount)
  narginchk(1, 3);
  nargoutchk(0, 1);
  expr = pattern.regexOf(basePattern);
  if nargin == 1
    quantifier = '*?';
  elseif nargin == 3
    count = [count, maxCount];
  end
  if nargin > 1 && isscalar(count)
    quantifier = ['{', int2str(count), ',}?'];
  elseif nargin > 1
    quantifier = ['{', int2str(count(1)), ',', quantifierUpper(count(2)), '}?'];
  end
  pat = pattern(['(?:', expr, ')', quantifier]);
end
%=============================================================================
function txt = quantifierUpper(value)
  if isinf(value)
    txt = '';
  else
    txt = int2str(value);
  end
end
%=============================================================================
