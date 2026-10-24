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
%=============================================================================
% missing assigned into a string array gives <missing> elements
c = strings(1, 1);
c(:) = missing;
assert_isequal(class(c), 'string');
assert_isequal(size(c), [1 1]);
assert_istrue(ismissing(c));
%=============================================================================
c = strings(1, 1);
c(2:3) = missing;
assert_isequal(class(c), 'string');
assert_isequal(ismissing(c), [false true true]);
%=============================================================================
c = strings(2, 3);
c(:) = missing;
assert_isequal(class(c), 'string');
assert_isequal(size(c), [2 3]);
assert_istrue(all(ismissing(c(:))));
%=============================================================================
c = strings(2, 2);
c(:, 1) = missing;
assert_isequal(ismissing(c), [true false; true false]);
%=============================================================================
% the new elements of a grown string array are <missing>
c = strings(0, 0);
c(3) = missing;
assert_isequal(ismissing(c), [true true true]);
%=============================================================================
% missing becomes NaN in a floating-point array
c = 1;
c(:) = missing;
assert_isequal(class(c), 'double');
assert_isequal(c, NaN);
%=============================================================================
c = 1;
c(1) = missing;
assert_isequal(c, NaN);
%=============================================================================
c = [1 2];
c(:) = missing;
assert_isequal(c, [NaN NaN]);
%=============================================================================
c = [1 2];
c(2:3) = missing;
assert_isequal(c, [1 NaN NaN]);
%=============================================================================
c = [1 2];
c(end + 1) = missing;
assert_isequal(c, [1 2 NaN]);
%=============================================================================
c = [1 2];
c([true false]) = missing;
assert_isequal(c, [NaN 2]);
%=============================================================================
c = [];
c(2) = missing;
assert_isequal(c, [0 NaN]);
%=============================================================================
c = single([1 2]);
c(1) = missing;
assert_isequal(c, single([NaN 2]));
%=============================================================================
c = single(1);
c(:) = missing;
assert_isequal(c, single(NaN));
%=============================================================================
c = sparse([1 2]);
c(1) = missing;
assert_istrue(issparse(c));
assert_isequal(full(c), [NaN 2]);
%=============================================================================
% A(:) = B with the same size keeps the class of A
c = int8(1);
c(:) = 2.7;
assert_isequal(c, int8(3));
%=============================================================================
c = 'a';
c(:) = 66;
assert_isequal(c, 'B');
%=============================================================================
c = strings(1, 1);
c(:) = single(7);
assert_isequal(c, "7");
%=============================================================================
% other classes cannot hold missing
msg = 'Unable to perform assignment because value of type ''missing'' is not convertible to ''char''.';
asserts.checkerror('c = ''ab''; c(:) = missing;', msg, 'Nelson:types:assignmentNotConvertible');
asserts.checkerror('c = ''a''; c(:) = missing;', msg);
asserts.checkerror('c = ''ab''; c(1, :) = missing;', msg);
msg = 'Unable to perform assignment because value of type ''missing'' is not convertible to ''cell''.';
asserts.checkerror('c = {1, 2}; c(:) = missing;', msg);
asserts.checkerror('c = {1}; c(:) = missing;', msg);
asserts.checkerror('c = {}; c(1) = missing;', msg);
msg = 'Unable to perform assignment because value of type ''missing'' is not convertible to ''int8''.';
asserts.checkerror('c = int8([1 2]); c(1) = missing;', msg);
asserts.checkerror('c = int8(1); c(:) = missing;', msg);
msg = 'Unable to perform assignment because value of type ''missing'' is not convertible to ''logical''.';
asserts.checkerror('c = true(1, 2); c(2:3) = missing;', msg);
asserts.checkerror('c = struct(''a'', 1); c(1) = missing;', 'Assignment between unlike types is not allowed.');
%=============================================================================
% a missing array only holds missing
c = missing;
c(3) = missing;
assert_isequal(class(c), 'missing');
assert_isequal(size(c), [1 3]);
msg = 'Unable to perform assignment because value of type ''double'' is not convertible to ''missing''.';
asserts.checkerror('c = missing; c(:) = 5;', msg);
asserts.checkerror('c = missing; c(2) = 5;', msg);
msg = 'Unable to perform assignment because value of type ''string'' is not convertible to ''missing''.';
asserts.checkerror('c = missing; c(:) = "zz";', msg);
%=============================================================================
% the right-hand side can be a missing array
m = missing;
m(2) = missing;
c = [1 2 3];
c(1:2) = m;
assert_isequal(c, [NaN NaN 3]);
c = single([1 2 3]);
c(2:3) = m;
assert_isequal(c, single([1 NaN NaN]));
%=============================================================================
