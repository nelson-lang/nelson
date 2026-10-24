%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function called on a direct bytecode path (leaf function, or a function
% calling itself) runs without a scope of its own. The context scope of the
% calling function must survive such a call: functions reading it (varlock,
% varunlock, evalin, ...) still find the caller's variables afterwards.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
asserts.isequal(directMacroCallCallerScopeCase(), 43);
asserts.isequal(directMacroCallRecursiveCase(3), 3);
%=============================================================================
% an anonymous function keeps the closure variables its nested callee updates:
% its scope is read back after the body ran, so it must not be emptied first
h = directMacroCallClosureFactory(10);
asserts.isequal(h(2), 12);
asserts.isequal(h(3), 15);
%=============================================================================
