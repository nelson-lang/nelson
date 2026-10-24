%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A colon range used as the operand of a higher-precedence operator must keep
% its parentheses when the anonymous function body is rebuilt.
%=============================================================================
f = @(x) (1:5) * x;
asserts.isequal(func2str(f), '@(x)(1:5)*x');
asserts.isequal(f(2), [2 4 6 8 10]);
%=============================================================================
f = @(x) x * (1:3);
asserts.isequal(func2str(f), '@(x)x*(1:3)');
asserts.isequal(f(2), [2 4 6]);
%=============================================================================
f = @(x) (x:5)';
asserts.isequal(func2str(f), '@(x)(x:5)''');
asserts.isequal(f(3), [3; 4; 5]);
%=============================================================================
f = @(x) - (1:x);
asserts.isequal(func2str(f), '@(x)-(1:x)');
asserts.isequal(f(3), [-1 -2 -3]);
%=============================================================================
f = @(x) ~ (0:x);
asserts.isequal(func2str(f), '@(x)~(0:x)');
asserts.isequal(f(2), [true false false]);
%=============================================================================
f = @(x) (1:x) + 1;
asserts.isequal(func2str(f), '@(x)(1:x)+1');
asserts.isequal(f(3), [2 3 4]);
%=============================================================================
f = @(x) (1:x) .^ 2;
asserts.isequal(func2str(f), '@(x)(1:x).^2');
asserts.isequal(f(3), [1 4 9]);
%=============================================================================
f = @(x) sum((1:x) * 2);
asserts.isequal(func2str(f), '@(x)sum((1:x)*2)');
asserts.isequal(f(3), 12);
%=============================================================================
% Unchanged forms: top level, stepped range, index argument, matrix element.
f = @(x) 1:2:x;
asserts.isequal(func2str(f), '@(x)1:2:x');
asserts.isequal(f(7), [1 3 5 7]);
%=============================================================================
f = @(x) 1:x + 1;
asserts.isequal(func2str(f), '@(x)1:x+1');
asserts.isequal(f(2), [1 2 3]);
%=============================================================================
f = @(x) (1:2:x) * 2;
asserts.isequal(func2str(f), '@(x)(1:2:x)*2');
asserts.isequal(f(5), [2 6 10]);
%=============================================================================
f = @(x) x(2:end);
asserts.isequal(func2str(f), '@(x)x(2:end)');
asserts.isequal(f([4 5 6]), [5 6]);
%=============================================================================
f = @(x) [1:x];
asserts.isequal(func2str(f), '@(x)[1:x]');
asserts.isequal(f(3), [1 2 3]);
%=============================================================================
f = str2func('@(x) (1:5)*x');
asserts.isequal(func2str(f), '@(x)(1:5)*x');
asserts.isequal(f(3), [3 6 9 12 15]);
%=============================================================================
f = @(x) sin((1:5) * x);
asserts.isequal(func2str(f), '@(x)sin((1:5)*x)');
asserts.isequal(f(2), sin([2 4 6 8 10]));
%=============================================================================
f = @(x) x((1:2) + 1, 1:end);
asserts.isequal(func2str(f), '@(x)x((1:2)+1,1:end)');
asserts.isequal(f(magic(3)), [3 5 7; 4 9 2]);
%=============================================================================
f = @(x) (1:x) == 2;
asserts.isequal(func2str(f), '@(x)(1:x)==2');
asserts.isequal(f(3), [false true false]);
%=============================================================================
% A prefix operator under a power or a transpose keeps its parentheses.
f = @(x) (-x) ^ 2;
asserts.isequal(func2str(f), '@(x)(-x)^2');
asserts.isequal(f(2), 4);
%=============================================================================
f = @(x) (-x) .^ 2;
asserts.isequal(func2str(f), '@(x)(-x).^2');
asserts.isequal(f([1 2]), [1 4]);
%=============================================================================
f = @(x) (~x)';
asserts.isequal(func2str(f), '@(x)(~x)''');
asserts.isequal(f([0 1]), [true; false]);
%=============================================================================
f = @(x) -x ^ 2;
asserts.isequal(func2str(f), '@(x)-x^2');
asserts.isequal(f(2), -4);
%=============================================================================
f = @(x) 2 ^ -x;
asserts.isequal(func2str(f), '@(x)2^-x');
asserts.isequal(f(1), 0.5);
%=============================================================================
