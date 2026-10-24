%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a class object used as an index is converted by its subsindex method
% (zero-based result), defined by its own class or inherited
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
b = ClassdefOperatorFolderBase(2);
d = ClassdefOperatorFolderDerived(3);
assert_isequal(subsindex(b), 1);
%=============================================================================
x = [10 20 30];
assert_isequal(x(b), 20);
assert_isequal(x(d), 30);
assert_isequal(x(1, b), 20);
m = [1 2 3; 4 5 6];
assert_isequal(m(b, d), 6);
assert_isequal(m(b, :), [4 5 6]);
%=============================================================================
s.x = x;
assert_isequal(s.x(b), 20);
assert_isequal(s.x(d), 30);
%=============================================================================
f = @(v, k) v(k);
assert_isequal(f(x, b), 20);
assert_isequal(f(x, d), 30);
%=============================================================================
c = {'a', 'b', 'c'};
assert_isequal(c{b}, 'b');
assert_isequal(c{d}, 'c');
assert_isequal(c(b), {'b'});
%=============================================================================
y = x;
y(b) = 99;
assert_isequal(y, [10 99 30]);
y(d) = 77;
assert_isequal(y, [10 99 77]);
y(1, b) = 5;
assert_isequal(y, [10 5 77]);
%=============================================================================
c{b} = 'B';
c{d} = 'C';
assert_isequal(c, {'a', 'B', 'C'});
c(b) = {'bb'};
assert_isequal(c, {'a', 'bb', 'C'});
%=============================================================================
s.x(b) = 0;
assert_isequal(s.x, [10 0 30]);
%=============================================================================
st = struct('f', {1, 2, 3});
st(b).f = 9;
assert_isequal([st.f], [1 9 3]);
assert_isequal(st(d).f, 3);
%=============================================================================
c = {'a', 'b', 'c'};
[c{b}] = deal('z');
assert_isequal(c, {'a', 'z', 'c'});
[c{b}, c{d}] = deal('y', 'w');
assert_isequal(c, {'a', 'y', 'w'});
c = {'a', 'b'; 'c', 'd'};
v = 'Q';
c{b, 1} = v;
c{d, 2} = 'K';
assert_isequal(c, {'a', 'b'; 'Q', 'd'; [], 'K'});
%=============================================================================
% a classic object indexing its own data field inside its methods
h = ClassdefClassicFieldHolder([10 20 30]);
assert_isequal(getAt(h, b), 20);
assert_isequal(getAt(h, d), 30);
h = setAt(h, b, 99);
assert_isequal(getAt(h, 2), 99);
h2 = ClassdefClassicFieldHolder([1 2 3; 4 5 6]);
assert_isequal(getAt2(h2, b, d), 6);
assert_isequal(getAt(h2, b, ':'), [4 5 6]);
%=============================================================================
% the subsindex result can hold several positions or none
x = [10 20 30 40];
assert_isequal(x(ClassdefSubsindexValue([0 2])), [10 30]);
assert_isequal(x(ClassdefSubsindexValue(int8([3; 1]))), [40 20]);
assert_isequal(size(x(ClassdefSubsindexValue([]))), [0 0]);
assert_isequal(size(x(ClassdefSubsindexValue(zeros(1, 0)))), [1 0]);
y = x;
y(ClassdefSubsindexValue([0 2])) = 0;
assert_isequal(y, [0 20 0 40]);
c = {'a', 'b', 'c'};
[c{ClassdefSubsindexValue([0 1])}] = deal('p', 'q');
assert_isequal(c, {'p', 'q', 'c'});
%=============================================================================
% a subsindex value that is not a zero-based integer position is an invalid index
msg = _('index must either be real positive integers or logicals.');
id = 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals';
assert_checkerror('x(ClassdefSubsindexValue(1.5))', msg, id);
assert_checkerror('x(ClassdefSubsindexValue(-1))', msg, id);
assert_checkerror('x(ClassdefSubsindexValue(NaN))', msg, id);
assert_checkerror('x(ClassdefSubsindexValue({1}))', msg, id);
assert_checkerror('x(ClassdefSubsindexValue(1i))', msg, id);
msg = _('Zero or negative index encountered.');
id = 'Nelson:types:invalidIndex:zeroNegativeIndexEncountered';
assert_checkerror('y(ClassdefSubsindexValue(-1)) = 1', msg, id);
%=============================================================================
