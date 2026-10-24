%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% horzcat / vertcat / cat methods defined by a superclass (@folder files or the
% classdef methods block) are dispatched for a subclass instance by the
% concatenation brackets and by the function forms
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
d = ClassdefOperatorFolderDerived(2);
b = ClassdefOperatorFolderBase(1);
D = 'ClassdefOperatorFolderDerived';
B = 'ClassdefOperatorFolderBase';
%=============================================================================
assert_isequal([d, d], ['horzcat:', D, ':', D]);
assert_isequal([d; d], ['vertcat:', D, ':', D]);
assert_isequal([d, d, d], ['horzcat:', D, ':', D, ':', D]);
assert_isequal(horzcat(d, d), ['horzcat:', D, ':', D]);
assert_isequal(vertcat(d, d), ['vertcat:', D, ':', D]);
%=============================================================================
assert_isequal([d, b], ['horzcat:', D, ':', B]);
assert_isequal([b, d], ['horzcat:', B, ':', D]);
assert_isequal([b; d], ['vertcat:', B, ':', D]);
assert_isequal([d; b], ['vertcat:', D, ':', B]);
%=============================================================================
assert_isequal([d, 1], ['horzcat:', D, ':double']);
assert_isequal([1, d], ['horzcat:double:', D]);
assert_isequal([d; 1], ['vertcat:', D, ':double']);
assert_isequal(vertcat(1, d), ['vertcat:double:', D]);
%=============================================================================
assert_isequal(cat(1, d, d), ['cat1:', D, ':', D]);
assert_isequal(cat(2, d, 1), ['cat2:', D, ':double']);
assert_isequal(cat(3, 1, d), ['cat3:double:', D]);
%=============================================================================
a = ClassdefOperatorInherit(3);
p = ClassdefOperatorInheritBase();
A = 'ClassdefOperatorInherit';
P = 'ClassdefOperatorInheritBase';
assert_isequal([a, a], ['horzcat:', A, ':', A]);
assert_isequal([a; a], ['vertcat:', A, ':', A]);
assert_isequal(horzcat(a, a), ['horzcat:', A, ':', A]);
assert_isequal(vertcat(a, 2), ['vertcat:', A, ':double']);
assert_isequal([a, p], ['horzcat:', A, ':', P]);
assert_isequal([2; a], ['vertcat:double:', A]);
assert_isequal([p, p], ['horzcat:', P, ':', P]);
%=============================================================================
% a subclass without any concatenation method still builds an object array
c = ClassdefInheritedIndexChild();
assert_isequal(class([c, c]), 'ClassdefInheritedIndexChild');
assert_isequal(size([c; c; c]), [3 1]);
%=============================================================================
