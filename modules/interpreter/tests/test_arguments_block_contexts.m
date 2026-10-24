%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Characterization net for arguments blocks in contexts other than a plain
% top-level function: nested functions and classdef methods. Both are handled
% today by the same pre-lexer rewrite; this locks their behavior so a future
% native front-end must keep supporting them.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% Nested function: the arguments block of the nested function applies its
% default (b = 7), and the nested function still sees the outer variable (a).
asserts.isequal(arguments_edge_nested_function(), 107);
asserts.isequal(arguments_edge_nested_function(1), 8);
%=============================================================================
% Classdef method: positional validation plus a name-value option with a
% default. The obj entry carries no validation and passes through.
obj = ArgumentsInMethod();
asserts.isequal(obj.scale(5), 10);
asserts.isequal(obj.scale(5, 'Factor', 10), 50);
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
rmpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
