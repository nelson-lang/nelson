%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% TargetClass(obj) dispatches to obj's TargetClass converter method, not the
% TargetClass constructor.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
s = ClassdefConvertSource(5);
t = ClassdefConvertTarget(s);
assert_isequal(class(t), 'ClassdefConvertTarget');
assert_isequal(t.payload, 50);
%=============================================================================
% a plain constructor call (non-object argument) is unaffected
t2 = ClassdefConvertTarget(7);
assert_isequal(t2.payload, 7);
%=============================================================================
% copy construction (same class argument) uses the constructor, not a converter
t3 = ClassdefConvertTarget(t2);
assert_isequal(class(t3), 'ClassdefConvertTarget');
%=============================================================================
