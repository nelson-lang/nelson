%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleReassignCase()
  % reassignment inside a function frees the previous holder immediately
  global CLASSDEF_LIFECYCLE_LOG
  w = ClassdefLifecycle('f1');
  w = ClassdefLifecycle('f2');
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'after-reassign';
  w = 1;
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'after-overwrite';
end
