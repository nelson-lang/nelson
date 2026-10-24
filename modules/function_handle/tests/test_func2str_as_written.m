%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% func2str gives an anonymous function as written, without blanks, with the
% separators inside brackets written ',' and ';'
%=============================================================================
asserts.isequal(func2str(@(x) x + 1), '@(x)x+1');
asserts.isequal(func2str(@(x, y) x .* y + sin(x)), '@(x,y)x.*y+sin(x)');
asserts.isequal(func2str(@(x) x == 1 && x ~= 2 || x < 3), '@(x)x==1&&x~=2||x<3');
asserts.isequal(func2str(@(x) - x ^ 2), '@(x)-x^2');
asserts.isequal(func2str(@(x) x - -1), '@(x)x--1');
%=============================================================================
% parentheses and literals are kept as written
asserts.isequal(func2str(@(x) ((x))), '@(x)((x))');
asserts.isequal(func2str(@(x) (a .* b) + c), '@(x)(a.*b)+c');
asserts.isequal(func2str(@() 1.50), '@()1.50');
asserts.isequal(func2str(@() .5), '@().5');
asserts.isequal(func2str(@() 1e-3), '@()1e-3');
asserts.isequal(func2str(@() 0x1F), '@()0x1F');
asserts.isequal(func2str(@() disp('a b')), '@()disp(''a b'')');
asserts.isequal(func2str(@() "str ing"), '@()"str ing"');
asserts.isequal(func2str(@() 'it''s'), '@()''it''''s''');
%=============================================================================
% separators inside brackets
asserts.isequal(func2str(@(x) [x, 1; 2 3]), '@(x)[x,1;2,3]');
asserts.isequal(func2str(@(x) [1 -2]), '@(x)[1,-2]');
asserts.isequal(func2str(@(x) [1 - 2]), '@(x)[1-2]');
asserts.isequal(func2str(@(x) [1 -2 + 3]), '@(x)[1,-2+3]');
asserts.isequal(func2str(@(x) [x' x']), '@(x)[x'',x'']');
asserts.isequal(func2str(@(x) {x; 'a'}), '@(x){x;''a''}');
asserts.isequal(func2str(@(x) ["a" "b"]), '@(x)["a","b"]');
F = @() [1 2 % first row
         3 4];
asserts.isequal(func2str(F), '@()[1,2;3,4]');
F = @(x) x + ... continued
    2;
asserts.isequal(func2str(F), '@(x)x+2');
%=============================================================================
% indexing, nested handles, name=value arguments
asserts.isequal(func2str(@(x) x(1).y{2}(3)), '@(x)x(1).y{2}(3)');
asserts.isequal(func2str(@(s) s.(f)), '@(s)s.(f)');
asserts.isequal(func2str(@(x) x(end, :)'), '@(x)x(end,:)''');
asserts.isequal(func2str(@(x) f(@sin, x)), '@(x)f(@sin,x)');
asserts.isequal(func2str(@(x) struct(Name= x)), '@(x)struct(Name=x)');
F = @(x) @(y) x + y;
asserts.isequal(func2str(F), '@(x)@(y)x+y');
asserts.isequal(func2str(F(1)), '@(y)x+y');
F = @(x) cellfun(@(v) v * 2, x);
asserts.isequal(func2str(F), '@(x)cellfun(@(v)v*2,x)');
%=============================================================================
% str2func
asserts.isequal(func2str(str2func('@(x)  x  +  1')), '@(x)x+1');
asserts.isequal(func2str(str2func('@(x) [x  x]')), '@(x)[x,x]');
asserts.isequal(func2str(str2func('@(x) sin (x)')), '@(x)sin(x)');
%=============================================================================
% display
F = @(x) x + 1;
asserts.isequal(evalc('disp(F)'), ['    @(x)x+1', char(10), char(10)]);
C = {F};
asserts.isequal(evalc('disp(C)'), ['    {@(x)x+1}', char(10), char(10)]);
%=============================================================================
% the body runs as written
s.y = {1, [5 6 7]};
F = @(x) x(1).y{2}(3);
asserts.isequal(F(s), 7);
s = struct('a', [5 6]);
fn = 'a';
F = @(s) s.(fn)(2);
asserts.isequal(F(s), 6);
%=============================================================================
F = @(name) ['a(''', name, ''')'];
asserts.isequal(F('b'), 'a(''b'')');
F = @() "a""b";
asserts.isequal(F(), "a""b");
%=============================================================================
