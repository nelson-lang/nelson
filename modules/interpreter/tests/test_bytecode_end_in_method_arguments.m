%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% `end` inside the arguments of obj.method(...) belongs to the innermost indexed variable
%=============================================================================
root = [tempdir(), 'test_end_in_method_arguments_', strrep(num2str(now(), 12), '.', '_')];
mkdir(root);
mkdir([root, '/+endpkg']);
writelines({'classdef EndHandleProbe < handle', '  properties', '    p = 11:20;', ...
  '  end', '  methods', '    function r = f(obj, x)', '      r = numel(x);', '    end', ...
  '    function r = g(obj, a, b)', '      r = [numel(a), numel(b)];', '    end', ...
  '  end', 'end'}, [root, '/EndHandleProbe.m']);
writelines({'classdef EndValueProbe', '  properties', '    p = 11:20;', '  end', ...
  '  methods', '    function r = f(obj, x)', '      r = numel(x);', '    end', ...
  '  end', 'end'}, [root, '/EndValueProbe.m']);
writelines({'classdef StaticProbe', '  methods (Static)', '    function r = m(x)', ...
  '      r = numel(x);', '    end', '  end', 'end'}, [root, '/+endpkg/StaticProbe.m']);
writelines({'function r = endInMethodArgumentsFun(o, v, w)', ...
  '  r = [o.f(v(3:end)), o.f(w(v(end):end)), o.p(v(end)), o.p(end - 1), o.p(round(end / 2))];', ...
  'end'}, [root, '/endInMethodArgumentsFun.m']);
writelines({'classdef EndDependentProbe < handle', '  properties', '    count = 0;', '  end', ...
  '  properties (Dependent)', '    dep', '  end', '  methods', '    function r = get.dep(obj)', ...
  '      obj.count = obj.count + 1;', '      r = 101:110;', '    end', '  end', 'end'}, ...
  [root, '/EndDependentProbe.m']);
writelines({'classdef EndOverloadProbe', '  methods', '    function r = end(obj, k, n)', ...
  '      r = 4;', '    end', '    function r = subsref(obj, s)', '      r = s(1).subs{1};', ...
  '    end', '  end', 'end'}, [root, '/EndOverloadProbe.m']);
writelines({'classdef EndOverloadHolder', '  properties', '    e = EndOverloadProbe();', ...
  '  end', 'end'}, [root, '/EndOverloadHolder.m']);
writelines({'function r = endDependentFun(d)', '  r = d.dep(round(end / 2));', 'end'}, ...
  [root, '/endDependentFun.m']);
writelines({'function r = endAfterEvalFun(v)', '  eval(''x = v;'');', ...
  '  r = [x(3:end), x(round(end / 2))];', 'end'}, [root, '/endAfterEvalFun.m']);
writelines({'function r = endAfterLoadFun(filename)', '  load(filename);', ...
  '  r = [z(2:end), z(min(end, 2))];', 'end'}, [root, '/endAfterLoadFun.m']);
addpath(root);
%=============================================================================
v = 1:10;
w = 1:20;
o = EndHandleProbe();
assert_isequal(o.f(v(3:end)), 8);
assert_isequal(f(o, v(3:end)), 8);
assert_isequal(o.f(v(end)), 1);
assert_isequal(o.f(v(end - 2:end)), 3);
assert_isequal(o.g(v(2:end), w(end - 4:end)), [9, 5]);
assert_isequal(o.f(w(v(end):end)), 11);
r = o.f(v(3:end));
assert_isequal(r, 8);
%=============================================================================
assert_isequal(o.p(end), 20);
assert_isequal(o.p(2:end), 12:20);
assert_isequal(o.p(end - 1), 19);
assert_isequal(o.p(v(end)), 20);
assert_isequal(o.p(min(end, 3)), 13);
%=============================================================================
q = EndValueProbe();
assert_isequal(q.f(v(4:end)), 7);
assert_isequal(q.p(end), 20);
assert_isequal(q.p(3:end), 13:20);
%=============================================================================
assert_isequal(endpkg.StaticProbe.m(v(2:end)), 9);
%=============================================================================
s.fn = @numel;
assert_isequal(s.fn(v(2:end)), 9);
%=============================================================================
assert_isequal(endInMethodArgumentsFun(o, v, w), [8, 11, 20, 19, 15]);
assert_isequal(endInMethodArgumentsFun(q, v, w), [8, 11, 20, 19, 15]);
%=============================================================================
% captured variables of anonymous functions
g = @() o.f(v(3:end));
assert_isequal(g(), 8);
g = @() o.p(round(end / 2));
assert_isequal(g(), 15);
g = @() v(3:end);
assert_isequal(g(), 3:10);
g = @(x) o.f(x(2:end));
assert_isequal(g(v), 9);
%=============================================================================
% names unknown when the eval text is compiled
assert_isequal(eval('v(3:end)'), 3:10);
assert_isequal(eval('numel(v(3:end))'), 8);
assert_isequal(eval('v(end - 1:end)'), [9, 10]);
assert_isequal(eval('w(v(end):end)'), 10:20);
assert_isequal(eval('w(round(end / 2))'), 10);
assert_isequal(eval('w(min(end, 3))'), 3);
assert_isequal(eval('o.f(v(3:end))'), 8);
assert_isequal(eval('o.p(2:end)'), 12:20);
assert_isequal(eval('s.fn(v(2:end))'), 9);
assert_isequal(eval('endpkg.StaticProbe.m(v(2:end))'), 9);
assert_isequal(eval('o.p(round(end / 2))'), 15);
assert_isequal(eval('o.p(min(end, 3))'), 13);
assert_isequal(eval('q.p(round(end / 2))'), 15);
assert_isequal(eval('o.f(w(round(end / 2)))'), 1);
t.inner.data = 1:6;
assert_isequal(eval('t.inner.data(round(end / 2))'), 3);
assert_isequal(o.p(round(end / 2)), 15);
assert_isequal(t.inner.data(round(end / 2)), 3);
t.a.b.c = 1:8;
t.a.b.fn = @numel;
assert_isequal(eval('t.a.b.c(round(end / 2))'), 4);
assert_isequal(eval('t.a.b.fn(v(3:end))'), 8);
assert_isequal(t.a.b.c(round(end / 2)), 4);
%=============================================================================
% the property measured by `end` is read once, however many `end` it serves
d = EndDependentProbe();
r1 = d.dep(round(end / 2));
assert_isequal(d.count, 1);
d.count = 0;
r2 = d.dep(min(end, 2) + round(end / 2));
assert_isequal([r1, r2], [105, 107]);
assert_isequal(d.count, 1);
d.count = 0;
assert_isequal(eval('d.dep(min(end, 2) + round(end / 2))'), 107);
assert_isequal(d.count, 1);
d.count = 0;
assert_isequal(eval('d.dep(round(end / 2))'), 105);
assert_isequal(d.count, 1);
d.count = 0;
assert_isequal(d.dep(end), 110);
assert_isequal(d.count, 1);
d.count = 0;
assert_isequal(d.dep(min(end, 1), round(end / 2)), 105);
assert_isequal(d.count, 1);
d.count = 0;
assert_isequal(endDependentFun(d), 105);
assert_isequal(d.count, 1);
% an error in a later argument leaves no read value behind
try
  d.dep(round(end / 2), error('Nelson:test:stop', 'stop'));
catch
end
d.count = 0;
assert_isequal(d.dep(2), 102);
assert_isequal(d.count, 1);
try
  eval('d.dep(round(end / 2), error(''Nelson:test:stop'', ''stop''))');
catch
end
d.count = 0;
assert_isequal(eval('d.dep(2)'), 102);
assert_isequal(d.count, 1);
%=============================================================================
% a class property overloading end
h = EndOverloadHolder();
assert_isequal(h.e(round(end / 2)), 2);
assert_isequal(eval('h.e(round(end / 2))'), 2);
%=============================================================================
eval('x = v(4:end);');
assert_isequal(x, 4:10);
%=============================================================================
% variables created at run time in a function body by eval or load
assert_isequal(endAfterEvalFun(1:10), [3:10, 5]);
z = 21:25;
matname = [root, '/endAfterLoad.mat'];
save(matname, 'z');
assert_isequal(endAfterLoadFun(matname), [22:25, 22]);
try
  eval('numel(3:end)');
  identifier = '';
catch err
  identifier = err.identifier;
end
assert_isequal(identifier, 'Nelson:interpreter:illegalEnd');
%=============================================================================
clear('o', 'q', 'd', 'h', 'g');
rmpath(root);
rmdir(root, 's');
assert_isfalse(isfolder(root));
%=============================================================================
