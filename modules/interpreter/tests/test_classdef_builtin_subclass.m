%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% construction and identity
d = ClassdefDoubleBase(3, 'm');
asserts.isequal(class(d), 'ClassdefDoubleBase');
asserts.istrue(isa(d, 'double'));
asserts.istrue(isa(d, 'ClassdefDoubleBase'));
asserts.isequal(double(d), 3);
asserts.isequal(d.Unit, 'm');
%=============================================================================
% arithmetic keeps the subclass, conversion returns the base
y = d + 1;
asserts.isequal(class(y), 'ClassdefDoubleBase');
asserts.isequal(double(y), 4);
z = 2 * d;
asserts.isequal(class(z), 'ClassdefDoubleBase');
asserts.isequal(double(z), 6);
n = -d;
asserts.isequal(double(n), -3);
%=============================================================================
% predicates follow the base type
asserts.istrue(isnumeric(d));
asserts.istrue(isfloat(d));
asserts.isfalse(isinteger(d));
asserts.istrue(isreal(d));
%=============================================================================
% array data, size, and indexing
m = ClassdefDoubleBase([1 2; 3 4], 'kg');
asserts.isequal(size(m), [2 2]);
asserts.isequal(numel(m), 4);
asserts.isequal(length(m), 2);
e = m(2, 2);
asserts.isequal(class(e), 'ClassdefDoubleBase');
asserts.isequal(double(e), 4);
m(1, 1) = 9;
asserts.isequal(double(m(1, 1)), 9);
%=============================================================================
% transpose, reshape, sum keep the subclass
t = m';
asserts.isequal(double(t(1, 2)), 3);
r = reshape(m, 1, 4);
asserts.isequal(size(r), [1 4]);
s = sum(m);
asserts.isequal(class(s), 'ClassdefDoubleBase');
%=============================================================================
% comparisons return logicals
asserts.istrue(d < 5);
asserts.istrue(d == 3);
asserts.isfalse(d > 10);
%=============================================================================
% concatenation keeps the subclass
c = [d, ClassdefDoubleBase(7)];
asserts.isequal(class(c), 'ClassdefDoubleBase');
asserts.isequal(double(c), [3 7]);
%=============================================================================
% integer base
k = ClassdefInt32Base(5);
asserts.istrue(isa(k, 'int32'));
asserts.istrue(isinteger(k));
asserts.isfalse(isfloat(k));
asserts.isequal(int32(k), int32(5));
asserts.isequal(double(k), 5);
k2 = k + 1;
asserts.isequal(class(k2), 'ClassdefInt32Base');
asserts.isequal(int32(k2), int32(6));
%=============================================================================
% default construction has empty base data
d0 = ClassdefDoubleBase();
asserts.istrue(isempty(d0));
asserts.isequal(class(d0), 'ClassdefDoubleBase');
%=============================================================================
