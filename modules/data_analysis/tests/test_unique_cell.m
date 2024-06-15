%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = {'one','two','twenty-two','One','two'};
R = unique(A);
REF = { 'One',   'one',   'twenty-two',   'two'};
assert_isequal(R, REF);
%=============================================================================
A = {'dog','cat','fish','horse','dog ','fish '};
R = unique(A);
REF = {'cat'    'dog'    'dog '    'fish'    'fish '   'horse'};
assert_isequal(R, REF);
%=============================================================================
A = {'one', 'two';'twenty-two','One';'two', 'one'};
R = unique(A);
REF = {'One';'one';'twenty-two';'two'       };
assert_isequal(R, REF);
%=============================================================================
A = {'one', 'two';'twenty-two','One';'two', 'one'};
[R, ia, ic] = unique(A);
REF = {'One';'one';'twenty-two';'two'       };
REF_ia = [ 5;   1;    2;     3];
REF_ic = [ 2;   3;    4;      4;     1;     2];
assert_isequal(R, REF);
assert_isequal(ia, REF_ia);
assert_isequal(ic, REF_ic);
%=============================================================================
[C, ia, ic] = unique({'Foo','Bar','Foo'});
C_REF = {'Bar', 'Foo'};
assert_isequal(C, C_REF);
ia_REF = [2; 1];
assert_isequal(ia, ia_REF);
ic_REF = [2; 1; 2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'Foo','Bar','Foo'});
C_REF = {'Bar', 'Foo'};
assert_isequal(C, C_REF);
ia_REF = [2; 1];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'Foo','Bar','Foo'});
C_REF = {'Bar', 'Foo'};
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({'Foo','Bar','FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
assert_isequal(C, C_REF);
ia_REF = [ 2;   1;      3];
assert_isequal(ia, ia_REF);
ic_REF = [ 2;   1;      3];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'Foo','Bar','FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
assert_isequal(C, C_REF);
ia_REF = [ 2;   1;      3];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'Foo','Bar','FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
assert_isequal(C, C_REF);
ia_REF =  1;
assert_isequal(ia, ia_REF);
ic_REF = [1; 1; 1];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
assert_isequal(C, C_REF);
ia_REF =  1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(cell(1,0));
C_REF = cell(1,0);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
ic_REF = zeros(0, 1);
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(cell(1,0));
C_REF = cell(1,0);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(cell(1,0));
C_REF = cell(1,0);
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({});
C_REF = cell(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
ic = zeros(0, 1);
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({});
C_REF = cell(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique({});
C_REF = cell(0, 1);
assert_isequal(C, C_REF);
%=============================================================================
