%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = {'one', 'two', 'twenty-two', 'One', 'two'};
R = unique(A);
REF = {'One', 'one', 'twenty-two', 'two'};
asserts.isequal(R, REF);
%=============================================================================
A = {'dog', 'cat', 'fish', 'horse', 'dog ', 'fish '};
R = unique(A);
REF = {'cat' 'dog' 'dog ' 'fish' 'fish ' 'horse'};
asserts.isequal(R, REF);
%=============================================================================
A = {'one', 'two'; 'twenty-two', 'One'; 'two', 'one'};
R = unique(A);
REF = {'One'; 'one'; 'twenty-two'; 'two'};
asserts.isequal(R, REF);
%=============================================================================
A = {'one', 'two'; 'twenty-two', 'One'; 'two', 'one'};
[R, ia, ic] = unique(A);
REF = {'One'; 'one'; 'twenty-two'; 'two'};
REF_ia = [5; 1; 2; 3];
REF_ic = [2; 3; 4; 4; 1; 2];
asserts.isequal(R, REF);
asserts.isequal(ia, REF_ia);
asserts.isequal(ic, REF_ic);
%=============================================================================
[C, ia, ic] = unique({'Foo', 'Bar', 'Foo'});
C_REF = {'Bar', 'Foo'};
asserts.isequal(C, C_REF);
ia_REF = [2; 1];
asserts.isequal(ia, ia_REF);
ic_REF = [2; 1; 2];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'Foo', 'Bar', 'Foo'});
C_REF = {'Bar', 'Foo'};
asserts.isequal(C, C_REF);
ia_REF = [2; 1];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'Foo', 'Bar', 'Foo'});
C_REF = {'Bar', 'Foo'};
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({'ingots', 'alloys', 'ingots'}, 'stable');
C_REF = {'ingots', 'alloys'};
asserts.isequal(C, C_REF);
ia_REF = [1; 2];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 1];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia, ic] = unique({'Foo', 'Bar', 'FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
asserts.isequal(C, C_REF);
ia_REF = [2; 1; 3];
asserts.isequal(ia, ia_REF);
ic_REF = [2; 1; 3];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'Foo', 'Bar', 'FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
asserts.isequal(C, C_REF);
ia_REF = [2; 1; 3];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'Foo', 'Bar', 'FooBar'}');
C_REF = {'Bar'; 'Foo'; 'FooBar'};
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
ic_REF = [1; 1; 1];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique({'z'; 'z'; 'z'});
C_REF = {'z'};
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(cell(1, 0));
C_REF = cell(1, 0);
asserts.isequal(C, C_REF);
ia_REF = zeros(0, 1);
asserts.isequal(ia, ia_REF);
ic_REF = zeros(0, 1);
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(cell(1, 0));
C_REF = cell(1, 0);
asserts.isequal(C, C_REF);
ia_REF = zeros(0, 1);
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(cell(1, 0));
C_REF = cell(1, 0);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique({});
C_REF = cell(0, 1);
asserts.isequal(C, C_REF);
ia_REF = zeros(0, 1);
asserts.isequal(ia, ia_REF);
ic = zeros(0, 1);
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique({});
C_REF = cell(0, 1);
asserts.isequal(C, C_REF);
ia_REF = zeros(0, 1);
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique({});
C_REF = cell(0, 1);
asserts.isequal(C, C_REF);
%=============================================================================
