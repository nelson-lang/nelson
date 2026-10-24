%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: an anonymous function created in a script executed through
% run() may capture that script's local functions by reference. Because run()
% shares the caller workspace, such a handle (an onCleanup task, or a returned
% handle) outlives the transient definition created for the script. The captured
% local functions must remain valid and callable afterwards instead of being
% freed with the script, which previously caused a use-after-free at cleanup.
%=============================================================================
inner = [modulepath('interpreter', 'tests'), '/onCleanup/run_oncleanup_localfunction.m'];
%=============================================================================
% The captured handle stays callable after the script returns, and clearing the
% onCleanup object runs the captured local function.
R = evalc('check_run_oncleanup_localfunction(inner)');
assert_isequal(R, sprintf('cleanup executed\n'));
%=============================================================================
% The onCleanup fires (once) at natural workspace teardown too, not only on an
% explicit clear.
R = evalc('check_run_oncleanup_teardown(inner)');
assert_isequal(R, sprintf('cleanup executed\n'));
%=============================================================================
function check_run_oncleanup_localfunction(inner)
  run(inner); % defines c (onCleanup) and h in this scope
  assert_isequal(h(), 42); % captured local function still callable
  clear c; % fires onCleanup -> captured local function
end
%=============================================================================
function check_run_oncleanup_teardown(inner)
  run(inner);
  assert_isequal(h(), 42);
  % c is left to be released when this function scope is torn down.
end
%=============================================================================
