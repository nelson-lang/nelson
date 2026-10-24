%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An anonymous function only captures the variables its body refers to. A
% handle without captures is called without a scope of its own; that call
% must leave the caller's workspace untouched.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% only referenced variables are captured
a = 10;
unused = zeros(10, 10);
f = @(x) x + a;
s = functions(f);
asserts.isequal(fieldnames(s.workspace{1}), {'a'});
asserts.isequal(f(1), 11);
g = @(x) x + 1;
s = functions(g);
asserts.istrue(isempty(fieldnames(s.workspace{1})));
%=============================================================================
% quoted text and numeric literals are not variable references; a transpose
% quote does not start text
e5 = 3;
b = [1 2; 3 4];
h = @() numel('a b unused') + 1e5 + e5;
s = functions(h);
asserts.isequal(fieldnames(s.workspace{1}), {'e5'});
asserts.isequal(h(), 100013);
k = @() b' * a;
s = functions(k);
asserts.isequal(sort(fieldnames(s.workspace{1})), {'a'; 'b'});
asserts.isequal(k(), [10 30; 20 40]);
%=============================================================================
% a dynamic field name and a struct are both captured
st.name = 'value';
name = 'name';
m = @() st.(name);
asserts.isequal(m(), 'value');
s = functions(m);
asserts.isequal(sort(fieldnames(s.workspace{1})), {'name'; 'st'});
%=============================================================================
% a body calling a nested function captures the whole parent workspace
h = anonymousNestedWrapperFactory(10);
asserts.isequal(h(2), 12);
s = functions(h);
asserts.istrue(isfield(s.workspace{1}, 'base'));
%=============================================================================
% a scope-free call (no captures) keeps the locals of the calling function
asserts.isequal(anonymousFunctionScopeFreeCase(), 44);
asserts.isequal(anonymousFunctionScopeFreeRunCase(), 42);
%=============================================================================
% same from a script workspace, through cellfun / arrayfun and a direct call
keep = 7;
p = @(x) x * 2;
asserts.isequal(p(2), 4);
asserts.isequal(cellfun(@(x) x * 2, {1, 2}), [2 4]);
asserts.isequal(arrayfun(@(x) x - 1, [1 2]), [0 1]);
asserts.isequal(keep, 7);
%=============================================================================
