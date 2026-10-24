%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = directMacroCallRecursiveCase(n)
  % same for the direct self-recursive call
  keep = n;
  varlock('local', 'keep');
  if n > 0
    r = directMacroCallRecursiveCase(n - 1) + 1;
  else
    r = 0;
  end
  varunlock('local', 'keep');
  r = r + keep - n;
end
