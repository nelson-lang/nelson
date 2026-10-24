%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Script meant to be executed through run(): it registers an onCleanup whose
% anonymous function calls a script-local function, and exports an anonymous
% handle that also references a script-local function. Both captured definitions
% must stay valid after this script's transient definition is destroyed.
c = onCleanup(@() cleanup_local());
h = @() double_local(21);
%=============================================================================
function cleanup_local()
  disp('cleanup executed');
end
%=============================================================================
function r = double_local(x)
  r = x * 2;
end
%=============================================================================
