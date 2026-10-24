%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('exist'), -1);
asserts.isequal(nargout('exist'), 1);
%=============================================================================
r = exist('toto');
asserts.isequal(r, 0);
toto = 3;
asserts.isequal(exist('toto'), 1);
clear toto
asserts.isequal(exist('toto'), 0);
asserts.isequal(exist('toto', 'builtin'), 0);
asserts.isequal(exist('exist', 'builtin'), 0);
asserts.isequal(exist('who', 'builtin'), 5);
asserts.isequal(exist('exist', 'file'), 2);
asserts.isequal(exist('exist'), 2);
asserts.isequal(exist(nelsonroot(), 'dir'), 7);
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
asserts.isequal(exist('sum', 'builtin'), 5);
asserts.isequal(exist('common_functions_matrix', 'file'), 2);
%=============================================================================
