%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
F = @() (3);
R = func2str(F);
REF = '@()3';
assert_isequal(R, REF);
%=============================================================================
F = @(x) (3);
R = func2str(F);
REF = '@(x)3';
assert_isequal(R, REF);
%=============================================================================
F = @(x, y) (3);
R = func2str(F);
REF = '@(x,y)3';
assert_isequal(R, REF);
%=============================================================================
F = @(x) cos(x);
R = func2str(F);
REF = '@(x)cos(x)';
assert_isequal(R, REF);
%=============================================================================
F = @(x) cos(1) + cos(3);
R = func2str(F);
REF = '@(x)cos(1)+cos(3)';
assert_isequal(R, REF);
%=============================================================================
F = @(x) 1 + 3;
R = func2str(F);
REF = '@(x)1+3';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:3;
R = func2str(F);
REF = '@(x)1:3';
assert_isequal(R, REF);
%=============================================================================
F = @(x)[1:3];
R = func2str(F);
REF = '@(x)[1:3]';
assert_isequal(R, REF);
%=============================================================================
F = @(x)[1:cos(x)];
R = func2str(F);
REF = '@(x)[1:cos(x)]';
assert_isequal(R, REF);
%=============================================================================
F = @(x)cos(x):2;
R = func2str(F);
REF = '@(x)cos(x):2';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:cos(x);
R = func2str(F);
REF = '@(x)1:cos(x)';
assert_isequal(R, REF);
%=============================================================================
F = @(x)cos(sin(x));
R = func2str(F);
REF = '@(x)cos(sin(x))';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:3:4;
R = func2str(F);
REF = '@(x)1:3:4';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:2:3:4:4:5;
R = func2str(F);
REF = '@(x)1:2:3:4:4:5';
assert_isequal(R, REF);
%=============================================================================
a = 1.3;
b = .2;
c = 30;
parabola = @(x) a*x .^ 2 + b*x + c
clear a b c
x = 1;
R = parabola(x);
REF =  31.50;
assert_isequal(R, REF);
%=============================================================================
F = @(c) (integral(@(x) (x .^ 2 + c*x + 1),0,1));
R = func2str(F);
REF = '@(c)integral(@(x)(((x.^2)+(c*x))+1),0,1)';
assert_isequal(R, REF);
%=============================================================================
myfunction = @(x,y) (x^2 + y^2 + x*y);
x = 1;
y = 10;
R = myfunction(x,y);
REF = 111;
assert_isequal(R, REF)
%=============================================================================
F = @(x,y) ndgrid((-x:x/c:x),(-y:y/c:y));
R = func2str(F);
REF = '@(x,y)ndgrid(-x:(x/c):x,-y:(y/c):y)';
assert_isequal(R, REF);
%=============================================================================
