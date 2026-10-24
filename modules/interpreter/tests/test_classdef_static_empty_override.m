%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% a static empty method defined by the class replaces the generated one
obj = ClassdefCustomEmpty.empty(0, 3);
assert_isequal(class(obj), 'ClassdefCustomEmpty');
assert_isequal(size(obj), [1 1]);
assert_isequal(size(obj.Values), [0 3]);
obj = ClassdefCustomEmpty.empty();
assert_isequal(size(obj.Values), [0 0]);
%=============================================================================
% a subclass inherits it
obj = ClassdefCustomEmptyChild.empty(0, 2);
assert_isequal(class(obj), 'ClassdefCustomEmpty');
assert_isequal(size(obj.Values), [0 2]);
%=============================================================================
% classes without their own empty keep the generated one
obj = ClassdefAbstractConcreteSameProperty.empty(0, 4);
assert_isequal(size(obj), [0 4]);
%=============================================================================
rmpath(path_test);
%=============================================================================
