%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
F = @() (3);
R = func2str(F);
REF = '@() 3';
assert_isequal(R, REF);
%=============================================================================
F = @(x) (3);
R = func2str(F);
REF = '@(x) 3';
assert_isequal(R, REF);
%=============================================================================
F = @(x, y) (3);
R = func2str(F);
REF = '@(x,y) 3';
assert_isequal(R, REF);
%=============================================================================
F = @(x) cos(x);
R = func2str(F);
REF = '@(x) cos(x)';
assert_isequal(R, REF);
%=============================================================================
F = @(x) cos(1) + cos(3);
R = func2str(F);
REF = '@(x) cos(1)+cos(3)';
assert_isequal(R, REF);
%=============================================================================
F = @(x) 1 + 3;
R = func2str(F);
REF = '@(x) 1+3';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:3;
R = func2str(F);
REF = '@(x) 1:3';
assert_isequal(R, REF);
%=============================================================================
F = @(x)[1:3];
R = func2str(F);
REF = '@(x) [1:3]';
assert_isequal(R, REF);
%=============================================================================
F = @(x)[1:cos(x)];
R = func2str(F);
REF = '@(x) [1:cos(x)]';
assert_isequal(R, REF);
%=============================================================================
F = @(x)cos(x):2;
R = func2str(F);
REF = '@(x) cos(x):2';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:cos(x);
R = func2str(F);
REF = '@(x) 1:cos(x)';
assert_isequal(R, REF);
%=============================================================================
F = @(x)cos(sin(x));
R = func2str(F);
REF = '@(x) cos(sin(x))';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:3:4;
R = func2str(F);
REF = '@(x) 1:3:4';
assert_isequal(R, REF);
%=============================================================================
F = @(x)1:2:3:4:4:5;
R = func2str(F);
REF = '@(x) 1:2:3:4:4:5';
assert_isequal(R, REF);
%=============================================================================
a = 1.3;
b = .2;
c = 30;
parabola = @(x) a*x .^ 2 + b*x + c;
clear a b c
x = 1;
R = parabola(x);
REF =  31.50;
assert_isequal(R, REF);
%=============================================================================
F = @(c) (integral(@(x) (x .^ 2 + c*x + 1),0,1));
R = func2str(F);
REF = '@(c) integral(@(x) ((x.^2)+(c*x))+1,0,1)';
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
REF = '@(x,y) ndgrid(-x:(x/c):x,-y:(y/c):y)';
assert_isequal(R, REF);
%=============================================================================
F = @(x) x.a+x.b;
R = func2str(F);
REF = '@(x) x.a+x.b';
assert_isequal(R, REF);
%=============================================================================
F = @(x) x.('a') + x.('ffb');
R = func2str(F);
REF = '@(x) x.(''a'')+x.(''ffb'')';
assert_isequal(R, REF);
%=============================================================================
F = @(in1, in2) {in1+in2, in1-in2, max(in1, in2)};
R = func2str(F);
REF = '@(in1,in2) {in1+in2, in1-in2, max(in1,in2)}';
assert_isequal(R, REF);
%=============================================================================
F = @(a) [1,a,a+1,a+2,4,cos(a)];
R = func2str(F);
REF = '@(a) [1, a, a+1, a+2, 4, cos(a)]';
assert_isequal(R, REF);
%=============================================================================
F = @(x) x(1).z;
R = func2str(F);
REF = '@(x) x(1).z';
assert_isequal(R, REF);
%=============================================================================
F = @(a) a.b.c.d(3);
R = func2str(F);
REF = '@(a) a.b.c.d(3)';
assert_isequal(R, REF);
%=============================================================================
F = @(a) a(end-1:1+a);
R = func2str(F);
REF = '@(a) a((end-1):(1+a))';
assert_isequal(R, REF);
%=============================================================================
F = @(x,y) [x,1,x;1,x,1;y,1,y];
R = func2str(F);
REF = '@(x,y) [x, 1, x; 1, x, 1; y, 1, y]';
assert_isequal(R, REF);
%=============================================================================
F = @() {};
R = func2str(F);
REF = '@() {}';
assert_isequal(R, REF);
%=============================================================================
F = @() [];
R = func2str(F);
REF = '@() []';
assert_isequal(R, REF);
%=============================================================================
F = @() {[1, 2; 3 4], "A", 'B', ["A", "CC"]};
R = func2str(F);
REF = '@() {[1, 2; 3, 4], "A", ''B'', ["A", "CC"]}';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with nested function calls
F = @(x) cos(sin(x)) + tan(x);
R = func2str(F);
REF = '@(x) cos(sin(x))+tan(x)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with logical operations
F = @(x, y) x > y && y < x;
R = func2str(F);
REF = '@(x,y) (x>y)&&(y<x)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with inline matrix operations
F = @(x) [x, x^2, x^3];
R = func2str(F);
REF = '@(x) [x, x^2, x^3]';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with cell array output
F = @(a, b) {a+b, a-b, a*b};
R = func2str(F);
REF = '@(a,b) {a+b, a-b, a*b}';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with nested anonymous functions
F = @(x) @(y) x + y;
R = func2str(F);
REF = '@(x) @(y) x+y';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with structure field access
F = @(s) s.field1 + s.field2;
R = func2str(F);
REF = '@(s) s.field1+s.field2';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with indexing and conditional operation
F = @(x) x(1:2:end) + (x > 0);
R = func2str(F);
REF = '@(x) x(1:2:end)+(x>0)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with mixed data types
F = @(x, y) [x, y, {x + y}];
R = func2str(F);
REF = '@(x,y) [x, y, {x+y}]';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with ternary-like operation (if-else using conditional)
F = @(x) (x > 0) * 1 + (x <= 0) * -1;
R = func2str(F);
REF = '@(x) ((x>0)*1)+((x<=0)*-1)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with complex number handling
F = @(z) real(z) + imag(z) * 1i;
R = func2str(F);
REF = '@(z) real(z)+(imag(z)*1i)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with function handle input
F = @(func, x) func(x) + 1;
R = func2str(F);
REF = '@(func,x) func(x)+1';
assert_isequal(R, REF);
%=============================================================================
% Tricky anonymous function with escaped characters
F = @(str) sprintf('Hello\nWorld');
R = func2str(F);
REF = '@(str) sprintf(''Hello\nWorld'')';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with nested cell and matrix structure
F = @(x) {[1, 2; 3, 4], {x, x^2}};
R = func2str(F);
REF = '@(x) {[1, 2; 3, 4], {x, x^2}}';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function with multiple levels of nesting and logical operators
F = @(x) (x > 0) && (x < 10) || (x == 5);
R = func2str(F);
REF = '@(x) ((x>0)&&(x<10))||(x==5)';
assert_isequal(R, REF);
%=============================================================================
% Anonymous function using nested indexing and complex number operations
F = @(a, b) a(b(1):b(end)) .* conj(a);
R = func2str(F);
REF = '@(a,b) a(b(1):b(end)).*conj(a)';
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('@', sprintf(_('Expecting %s'),  _('function name or parameter list after ''@''')));
%=============================================================================
assert_checkerror('@(', sprintf(_('Expecting %s'),  _('argument list or closing parenthesis after ''(''')));
%=============================================================================
assert_checkerror('@(1', sprintf(_('Expecting %s'),  _('argument list or closing parenthesis after ''(''')));
%=============================================================================
assert_checkerror('@(a)', sprintf(_('Expecting %s'),  _('expression for anonymous function body after '')''')));
%=============================================================================
assert_checkerror('@()', sprintf(_('Expecting %s'),  _('expression for anonymous function body after '')''')));
%=============================================================================
  