%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
old_recursion_depth = max_recursion_depth();
cleanup = onCleanup(@() max_recursion_depth(old_recursion_depth));
max_recursion_depth(1);
did_catch = false;
try
  clear cleanup;
catch ex
  did_catch = true;
  asserts.istrue(contains(ex.message, _('Allowable stack depth exceeded...')));
end
max_recursion_depth(old_recursion_depth);
asserts.istrue(did_catch);
%=============================================================================
