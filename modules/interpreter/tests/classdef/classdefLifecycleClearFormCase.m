%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleClearFormCase()
  % both the command form and the function form of clear free the holder
  global CLASSDEF_LIFECYCLE_LOG
  a = ClassdefLifecycle('fa');
  b = ClassdefLifecycle('fb');
  clear('a');
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'after-clear-fn';
  clear b
  CLASSDEF_LIFECYCLE_LOG{end + 1} = 'after-clear-cmd';
end
