%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A nelson.mixin.Scalar subclass can only be scalar: concatenating instances
% into a non-scalar array is an error.
%=============================================================================
d = [tempname(), '/'];
mkdir(d);
%=============================================================================
filewrite([d, 'Sc.m'], [...
  "classdef Sc < nelson.mixin.Scalar"; ...
  "  properties"; ...
  "    V = 1"; ...
  "  end"; ...
  "end"]);
addpath(d);
%=============================================================================
a = Sc();
assert_istrue(isa(a, 'nelson.mixin.Scalar'));
assert_isequal(a.V, 1);
%=============================================================================
% horizontal concatenation into a non-scalar array is rejected
assert_checkerror('[Sc(), Sc()]', ...
  'Concatenating ''Sc'' objects is not supported because ''Sc'' objects can only be scalar.');
%=============================================================================
% vertical concatenation too
assert_checkerror('[Sc(); Sc()]', ...
  'Concatenating ''Sc'' objects is not supported because ''Sc'' objects can only be scalar.');
%=============================================================================
% parentheses-indexed assignment is rejected
c = Sc();
assert_checkerror('c(2) = Sc()', ...
  'Indexed assignment with parentheses is not supported for ''Sc'' objects.');
%=============================================================================
% repmat into a non-scalar array is rejected; repmat to a scalar is allowed
assert_checkerror('repmat(Sc(), 1, 2)', '''Sc'' objects can only be scalar.');
r = repmat(Sc(), 1, 1);
assert_isequal(r.V, 1);
%=============================================================================
% empty array of a scalar class is rejected
assert_checkerror('Sc.empty(0, 0)', ...
  'Creating an empty array of a scalar class is not supported.');
%=============================================================================
