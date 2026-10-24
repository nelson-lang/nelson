%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = anonymousFunctionLifetimeScopeCase()
  % the handle and its captured object are locals: both die on return
  obj = ClassdefLifecycle('scope');
  f = @() obj.Name;
  r = f();
end
