%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
F = @()(3);
R = func2str(F);
REF = '@()(3)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x)(3);
R = func2str(F);
REF = '@(x)(3)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x, y)(3);
R = func2str(F);
REF = '@(x,y)(3)';
asserts.isequal(R, REF);
%=============================================================================
F = @(~, ~) disp('3 seconds have elapsed');
R = func2str(F);
REF = '@(~,~)disp(''3 seconds have elapsed'')';
asserts.isequal(R, REF);
%=============================================================================
F = @(~, value, ~) value + 1;
R = func2str(F);
REF = '@(~,value,~)value+1';
asserts.isequal(R, REF);
R = F(10, 20, 30);
REF = 21;
asserts.isequal(R, REF);
%=============================================================================
F = @(x) cos(x);
R = func2str(F);
REF = '@(x)cos(x)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) cos(1) + cos(3);
R = func2str(F);
REF = '@(x)cos(1)+cos(3)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) 1 + 3;
R = func2str(F);
REF = '@(x)1+3';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) 1:3;
R = func2str(F);
REF = '@(x)1:3';
asserts.isequal(R, REF);
%=============================================================================
F = @(x)[1:3];
R = func2str(F);
REF = '@(x)[1:3]';
asserts.isequal(R, REF);
%=============================================================================
F = @(x)[1:cos(x)];
R = func2str(F);
REF = '@(x)[1:cos(x)]';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) cos(x):2;
R = func2str(F);
REF = '@(x)cos(x):2';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) 1:cos(x);
R = func2str(F);
REF = '@(x)1:cos(x)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) cos(sin(x));
R = func2str(F);
REF = '@(x)cos(sin(x))';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) 1:3:4;
R = func2str(F);
REF = '@(x)1:3:4';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) 1:2:3:4:4:5;
R = func2str(F);
REF = '@(x)1:2:3:4:4:5';
asserts.isequal(R, REF);
%=============================================================================
a = 1.3;
b = .2;
c = 30;
parabola = @(x) a * x .^ 2 + b * x + c;
clear a b c
x = 1;
R = parabola(x);
REF = 31.50;
asserts.isequal(R, REF);
%=============================================================================
F = @(c)(integral(@(x)(x .^ 2 + c * x + 1), 0, 1));
R = func2str(F);
REF = '@(c)(integral(@(x)(x.^2+c*x+1),0,1))';
asserts.isequal(R, REF);
%=============================================================================
myfunction = @(x, y)(x ^ 2 + y ^ 2 + x * y);
x = 1;
y = 10;
R = myfunction(x, y);
REF = 111;
asserts.isequal(R, REF)
%=============================================================================
identity = @(x) x;
s = 0;
for k = 1:1000
  s = s + identity(k);
end
asserts.isequal(s, 500500);
%=============================================================================
F = @(x, y) ndgrid((-x:x / c:x), (-y:y / c:y));
R = func2str(F);
REF = '@(x,y)ndgrid((-x:x/c:x),(-y:y/c:y))';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) x.a + x.b;
R = func2str(F);
REF = '@(x)x.a+x.b';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) x.('a') + x.('ffb');
R = func2str(F);
REF = '@(x)x.(''a'')+x.(''ffb'')';
asserts.isequal(R, REF);
%=============================================================================
F = @(in1, in2) {in1 + in2, in1 - in2, max(in1, in2)};
R = func2str(F);
REF = '@(in1,in2){in1+in2,in1-in2,max(in1,in2)}';
asserts.isequal(R, REF);
%=============================================================================
F = @(a)[1, a, a + 1, a + 2, 4, cos(a)];
R = func2str(F);
REF = '@(a)[1,a,a+1,a+2,4,cos(a)]';
asserts.isequal(R, REF);
%=============================================================================
F = @(x) x(1).z;
R = func2str(F);
REF = '@(x)x(1).z';
asserts.isequal(R, REF);
%=============================================================================
F = @(a) a.b.c.d(3);
R = func2str(F);
REF = '@(a)a.b.c.d(3)';
asserts.isequal(R, REF);
%=============================================================================
F = @(a) a(end - 1:1 + a);
R = func2str(F);
REF = '@(a)a(end-1:1+a)';
asserts.isequal(R, REF);
%=============================================================================
F = @(x, y)[x, 1, x; 1, x, 1; y, 1, y];
R = func2str(F);
REF = '@(x,y)[x,1,x;1,x,1;y,1,y]';
asserts.isequal(R, REF);
%=============================================================================
F = @() {};
R = func2str(F);
REF = '@(){}';
asserts.isequal(R, REF);
%=============================================================================
F = @()[];
R = func2str(F);
REF = '@()[]';
asserts.isequal(R, REF);
%=============================================================================
F = @() {[1, 2; 3 4], "A", 'B', ["A", "CC"]};
R = func2str(F);
REF = '@(){[1,2;3,4],"A",''B'',["A","CC"]}';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with nested function calls
F = @(x) cos(sin(x)) + tan(x);
R = func2str(F);
REF = '@(x)cos(sin(x))+tan(x)';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with logical operations
F = @(x, y) x > y && y < x;
R = func2str(F);
REF = '@(x,y)x>y&&y<x';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with inline matrix operations
F = @(x)[x, x ^ 2, x ^ 3];
R = func2str(F);
REF = '@(x)[x,x^2,x^3]';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with cell array output
F = @(a, b) {a + b, a - b, a * b};
R = func2str(F);
REF = '@(a,b){a+b,a-b,a*b}';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with nested anonymous functions
F = @(x) @(y) x + y;
R = func2str(F);
REF = '@(x)@(y)x+y';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with structure field access
F = @(s) s.field1 + s.field2;
R = func2str(F);
REF = '@(s)s.field1+s.field2';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with indexing and conditional operation
F = @(x) x(1:2:end) + (x > 0);
R = func2str(F);
REF = '@(x)x(1:2:end)+(x>0)';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with mixed data types
F = @(x, y)[x, y, {x + y}];
R = func2str(F);
REF = '@(x,y)[x,y,{x+y}]';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with ternary-like operation (if-else using conditional)
F = @(x)(x > 0) * 1 + (x <= 0) * -1;
R = func2str(F);
REF = '@(x)(x>0)*1+(x<=0)*-1';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with complex number handling
F = @(z) real(z) + imag(z) * 1i;
R = func2str(F);
REF = '@(z)real(z)+imag(z)*1i';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with function handle input
F = @(func, x) func(x) + 1;
R = func2str(F);
REF = '@(func,x)func(x)+1';
asserts.isequal(R, REF);
%=============================================================================
% Tricky anonymous function with escaped characters
F = @(str) sprintf('Hello\nWorld');
R = func2str(F);
REF = '@(str)sprintf(''Hello\nWorld'')';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with nested cell and matrix structure
F = @(x) {[1, 2; 3, 4], {x, x ^ 2}};
R = func2str(F);
REF = '@(x){[1,2;3,4],{x,x^2}}';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function with multiple levels of nesting and logical operators
F = @(x)(x > 0) && (x < 10) || (x == 5);
R = func2str(F);
REF = '@(x)(x>0)&&(x<10)||(x==5)';
asserts.isequal(R, REF);
%=============================================================================
% Anonymous function using nested indexing and complex number operations
F = @(a, b) a(b(1):b(end)) .* conj(a);
R = func2str(F);
REF = '@(a,b)a(b(1):b(end)).*conj(a)';
asserts.isequal(R, REF);
%=============================================================================
F = @(varargin) feval(@horzcat, varargin{:});
R = func2str(F);
REF = '@(varargin)feval(@horzcat,varargin{:})';
asserts.isequal(R, REF);
R = feval(F, 1, 2, 3);
REF = [1, 2, 3];
asserts.isequal(R, REF);
%=============================================================================
varargin = {10, 20};
F = @(src, event) feval(@horzcat, src, event, varargin{:});
R = func2str(F);
REF = '@(src,event)feval(@horzcat,src,event,varargin{:})';
asserts.isequal(R, REF);
R = feval(F, 1, 2);
REF = [1, 2, 10, 20];
asserts.isequal(R, REF);
clear varargin;
%=============================================================================
feval('asserts.throws', '@', _('function name or parameter list after ''@'''));
%=============================================================================
feval('asserts.throws', '@(', _('argument list or closing parenthesis after ''('''));
%=============================================================================
feval('asserts.throws', '@(1', _('argument list or closing parenthesis after ''('''));
%=============================================================================
feval('asserts.throws', '@(a)', _('expression for anonymous function body after '')'''));
%=============================================================================
feval('asserts.throws', '@()', _('expression for anonymous function body after '')'''));
%=============================================================================

% an anonymous function can access a multi-level field of a captured struct
capg.h = @(a) a * 2;
capq = @(a) capg.h(a);
asserts.isequal(capq(5), 10);
caps.M.inner = @(x, u, v) x + u + v;
capx = 10;
capf = @(u, v) caps.M.inner(capx, u, v);
asserts.isequal(capf(1, 2), 13);
capd.M.val = 42;
capw = @() capd.M.val;
asserts.isequal(capw(), 42);
%=============================================================================
% Unary minus applied to a parenthesized group must keep the parentheses when
% the body is serialized, otherwise "-(a+b)" silently evaluates as "-a+b".
F = @(s) - (s(2) + s(3));
R = func2str(F);
REF = '@(s)-(s(2)+s(3))';
asserts.isequal(R, REF);
p = [6; -8; 0.03];
asserts.isapprox(F(p), 7.97, 1e-10);
%=============================================================================
F = @(a, b) - (a + b);
asserts.isequal(func2str(F), '@(a,b)-(a+b)');
asserts.isequal(F(2, 3), -5);
%=============================================================================
F = @(a, b) - (a - b);
asserts.isequal(func2str(F), '@(a,b)-(a-b)');
asserts.isequal(F(2, 3), 1);
%=============================================================================
% Unary not applied to a parenthesized logical group.
F = @(a, b) ~ (a & b);
asserts.isequal(func2str(F), '@(a,b)~(a&b)');
asserts.isequal(F(true, false), true);
%=============================================================================
% Unary minus of a single term must not gain spurious parentheses.
F = @(x) - x;
asserts.isequal(func2str(F), '@(x)-x');
asserts.isequal(F(4), -4);
%=============================================================================
