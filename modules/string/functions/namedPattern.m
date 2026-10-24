%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = namedPattern(basePattern, name, description)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if nargin < 2
    name = '';
  end
  if isstring(name)
    name = char(name);
  end
  if nargin >= 3 && isstring(description)
    description = char(description);
  end
  if isempty(name)
    pat = pattern(pattern.regexOf(basePattern), 'namedPattern');
    return
  end
  pat = pattern(['(?<', name, '>', pattern.regexOf(basePattern), ')'], name);
end
%=============================================================================
