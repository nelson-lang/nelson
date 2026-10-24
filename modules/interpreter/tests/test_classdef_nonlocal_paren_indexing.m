%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% "()" indexing of a class object held in a non-local (global) variable must
% dispatch to subsref, for single and multiple outputs alike.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
global g_obj
g_obj = ClassdefParenMultiOutput([10 20 30 40]);
%=============================================================================
% single output through the non-local variable
asserts.isequal(g_obj([2 4]), 20);
%=============================================================================
% two outputs
[a, b] = g_obj([1 3]);
asserts.isequal(a, 10);
asserts.isequal(b, 30);
%=============================================================================
% three outputs
[a, b, c] = g_obj([1 2 4]);
asserts.isequal(a, 10);
asserts.isequal(b, 20);
asserts.isequal(c, 40);
%=============================================================================
clear global g_obj
%=============================================================================
