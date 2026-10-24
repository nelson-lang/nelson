%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleScopeCase()
  % three locals die at scope exit, in declaration order
  global CLASSDEF_LIFECYCLE_LOG
  a = ClassdefLifecycle('s1');
  b = ClassdefLifecycle('s2');
  c = ClassdefLifecycle('s3');
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'body-done';
end
