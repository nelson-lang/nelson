%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An anonymous function body holding another anonymous function, followed by
% more statements on the same line, in a script and in a function file.
%=============================================================================
h = @() feval(@() 3); x = h(); assert_isequal(x, 3);
h = @() feval(@() 3), x = h(), assert_isequal(x, 3);
assert_isequal(func2str(h), '@()feval(@()3)');
h = @(a) feval(@() a + 1); assert_isequal(h(2), 3);
h = @() feval(@(b) b * 2, 4); assert_isequal(h(), 8);
h = @(a) feval(@(b) a + b, 10); assert_isequal(h(1), 11);
h = @() @() 5; g = h(); assert_isequal(g(), 5);
h = @() {@() 1, @(c) c}; c = h(); assert_isequal(c{1}(), 1); assert_isequal(c{2}(7), 7);
h = @() feval(@() feval(@() [1 3])); assert_isequal(h(), [1 3]);
h = @() cellfun(@(v) v + 1, {1, 2}); assert_isequal(h(), [2 3]);
%=============================================================================
d = [tempdir(), 'test_anonymous_nested_handles'];
if isdir(d)
  rmdir(d, 's');
end
mkdir(d);
lines = {'function r = anon_nested_fun()';
'  h = @() feval(@() 3); x = h(); r = {x};';
'  h = @(a) feval(@(b) a + b, 1), r{end + 1} = h(1);';
'  r{end + 1} = show(''label'', @() f(@() g([1 3])));';
'  q = @() show(''in'', @() f(@() g(2))); r{end + 1} = q();';
'end';
'function s = show(label, fh)';
'  s = {label, fh()};';
'end';
'function y = f(fh)';
'  y = fh();';
'end';
'function y = g(v)';
'  y = 2 * v;';
'end'};
filewrite([d, '/anon_nested_fun.m'], strjoin(lines, char(10)));
addpath(d);
r = anon_nested_fun();
rmpath(d);
rmdir(d, 's');
assert_isequal(r{1}, 3);
assert_isequal(r{2}, 2);
assert_isequal(r{3}, {'label', [2 6]});
assert_isequal(r{4}, {'in', 4});
%=============================================================================
