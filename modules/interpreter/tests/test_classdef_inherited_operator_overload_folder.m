%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% operator methods defined in the superclass @folder are dispatched for a
% subclass instance by every operator and its function form
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
d = ClassdefOperatorFolderDerived(2);
C = 'ClassdefOperatorFolderDerived';
%=============================================================================
assert_isequal(d & d, ['and:', C, ':', C]);
assert_isequal(d & true, ['and:', C, ':logical']);
assert_isequal(true & d, ['and:logical:', C]);
assert_isequal(and(d, d), ['and:', C, ':', C]);
assert_isequal(d | d, ['or:', C, ':', C]);
assert_isequal(or(d, 1), ['or:', C, ':double']);
%=============================================================================
assert_isequal(~d, ['not:', C]);
assert_isequal(not(d), ['not:', C]);
assert_isequal(+d, ['uplus:', C]);
assert_isequal(uplus(d), ['uplus:', C]);
assert_isequal(-d, ['uminus:', C]);
assert_isequal(d.', ['transpose:', C]);
assert_isequal(transpose(d), ['transpose:', C]);
assert_isequal(d', ['ctranspose:', C]);
assert_isequal(ctranspose(d), ['ctranspose:', C]);
%=============================================================================
assert_isequal(d:d, ['colon:', C, ':', C]);
assert_isequal(d:1:d, ['colon:', C, ':double']);
assert_isequal(colon(1, d), ['colon:double:', C]);
%=============================================================================
r = 0;
if d
  r = 1;
end
assert_isequal(r, 1);
assert_isequal(d && true, true);
assert_isequal(ClassdefOperatorFolderDerived(0) || false, false);
%=============================================================================
b = ClassdefOperatorFolderBase(2);
assert_isequal(b & b, 'and:ClassdefOperatorFolderBase:ClassdefOperatorFolderBase');
assert_isequal(~b, 'not:ClassdefOperatorFolderBase');
%=============================================================================
