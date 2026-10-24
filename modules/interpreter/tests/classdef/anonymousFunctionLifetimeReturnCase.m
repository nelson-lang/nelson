%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = anonymousFunctionLifetimeReturnCase()
  % the returned handle keeps its captured object alive past the scope
  obj = ClassdefLifecycle('returned');
  f = @() obj;
end
