%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function classdefLifecycleArgCase(h)
  % an argument is shared with the caller: nothing dies at scope exit
  global CLASSDEF_LIFECYCLE_LOG
  CLASSDEF_LIFECYCLE_LOG{end + 1} = ['arg:', h.Name];
end
