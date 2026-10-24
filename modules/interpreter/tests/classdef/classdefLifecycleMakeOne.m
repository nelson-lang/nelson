%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = classdefLifecycleMakeOne()
  % the returned handle survives the callee scope, the other local does not
  global CLASSDEF_LIFECYCLE_LOG
  tmp = ClassdefLifecycle('local');
  r = ClassdefLifecycle('ret');
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'body-done';
end
