%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% ClassName.staticMethod(...) at script top level / immediate (-e) mode must
% dispatch to the static method, exactly as it does inside a compiled function
% body. When the class directory is put on the path by an earlier statement in
% the same compiled chunk, the class is not loadable when the call site is
% compiled, so the call must defer to the runtime classdef member dispatch
% rather than being lowered to a construct then index (which, for a class with a
% guarded constructor, would raise or, for a recursive constructor, loop).
%=============================================================================
subdir = [modulepath('interpreter', 'tests'), '/classdef/deferred_static'];
%=============================================================================
% Deferred path: the addpath and the static call live in the same compiled
% chunk, so the class is not on the path when the call site is compiled.
cmd = ['addpath(''', subdir, '''); deferred_result = deferredstatichost.scale(6);'];
eval(cmd);
assert_isequal(deferred_result, 60);
%=============================================================================
% Eager path: the class is now known, so the compile-time static dispatch runs.
assert_isequal(deferredstatichost.scale(7), 70);
%=============================================================================
% The static dispatch above never went through the (guarded) constructor: a
% direct zero-argument construction still raises the guard error.
assert_checkerror('deferredstatichost()', ...
  'deferredstatichost must not be default-constructed');
%=============================================================================
rmpath(subdir);
%=============================================================================
