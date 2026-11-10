%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
t = ClassdefTemplateImpl();
%=============================================================================
% a superclass method reaches the protected override of its own declaration
asserts.isequal(callHook(t, 1), 'hook 1');
asserts.isequal(t.callHook(2), 'hook 2');
asserts.isequal(t.callHookDot(3), 'hook 3');
asserts.isequal(callStep(t), 'impl step');
% the inherited subsref also reaches it
asserts.isequal(t.abc, 'hook abc');
% outside the hierarchy the protected method stays hidden
asserts.checkerror('hook(t, 4)', 'No such property: hook');
%=============================================================================
% display and disp are inherited by the subclass, called directly or implicitly
txt = evalc('t');
asserts.isequal(txt, sprintf('template display\ntemplate disp ClassdefTemplateImpl\n'));
asserts.isequal(evalc('display(t)'), txt);
asserts.isequal(evalc('disp(t)'), sprintf('template disp ClassdefTemplateImpl\n'));
%=============================================================================
% concatenation goes through the inherited overloads
asserts.checkerror('[t, t]', 'no concatenation');
asserts.checkerror('[t; t]', 'no concatenation');
asserts.checkerror('horzcat(t, t)', 'no concatenation');
%=============================================================================
