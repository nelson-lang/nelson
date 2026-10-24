%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A named function handle can target a dotted name, in particular a classdef
% static method (@Class.staticMethod). The handle keeps the full dotted name and
% dispatches to the static method when invoked.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% The handle keeps the full dotted name.
h = @ClassdefStaticHandleTarget.scale;
assert_isequal(class(h), 'function_handle');
assert_isequal(func2str(h), 'ClassdefStaticHandleTarget.scale');
%=============================================================================
% Invoking the handle calls the static method.
assert_isequal(h(4), 40);
%=============================================================================
% Usable like any other handle.
assert_isequal(arrayfun(@ClassdefStaticHandleTarget.scale, [1 2 3]), [10 20 30]);
%=============================================================================
% A deeper dotted name is kept intact.
assert_isequal(func2str(@a.b.c), 'a.b.c');
%=============================================================================
