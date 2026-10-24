%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = anonymousFunctionScopeFreeCase()
  % a handle without captures runs without a scope of its own: the locals
  % of the calling function must survive the call (varunlock errors if the
  % variable vanished)
  keep = 40;
  varlock('local', 'keep');
  f = @(x) x + 1;
  r = f(1);
  r = r + cellfun(@(x) x * 2, {1}) + arrayfun(@(x) x - 1, 1);
  varunlock('local', 'keep');
  r = r + keep;
end
