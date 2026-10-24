%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function oncleanup_recursion_probe(n)
  c = onCleanup(@() disp(sprintf('cleanup %d', n)));
  if n > 1
    oncleanup_recursion_probe(n - 1);
  end
  disp(sprintf('end %d', n));
end
%=============================================================================
