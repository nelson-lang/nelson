%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
previous = cd(folder);
cleanup = onCleanup(@() cd(previous));
addpath(folder);
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% value and handle classes defined for this test
fid = fopen(fullfile(folder, 'Nh5Point.m'), 'wt');
fprintf(fid, 'classdef Nh5Point\n  properties\n    X = 0\n    Y = 0\n  end\n');
fprintf(fid, '  methods\n    function obj = Nh5Point(x, y)\n      if nargin > 0\n');
fprintf(fid, '        obj.X = x;\n        obj.Y = y;\n      end\n    end\n  end\nend\n');
fclose(fid);
fid = fopen(fullfile(folder, 'Nh5Counter.m'), 'wt');
fprintf(fid, 'classdef Nh5Counter < handle\n  properties\n    Count = 0\n  end\nend\n');
fclose(fid);
%=============================================================================
d = dictionary(["a", "b", "c"], [1 2 3]);
dc = dictionary([10 20], {struct('p', 1), 'text'});
mi = repmat(missing, 3, 2);
p = Nh5Point(3, 4);
pa = [Nh5Point(1, 2), Nh5Point(5, 6)];
h = Nh5Counter();
h.Count = 7;
k = ClassdefSaveLoadHooks(7, 'nh5');
savenh5('objects.nh5', 'd', 'dc', 'mi', 'p', 'pa', 'h', 'k');
ref = loadnh5('objects.nh5');
m = nh5file('objects.nh5', 'Writable', true);
%=============================================================================
% whole reads give the same values as loadnh5
asserts.isequal(m.d, ref.d);
asserts.isequal(class(m.d), 'dictionary');
asserts.isequal(lookup(m.d, "b"), 2);
asserts.isequal(m.dc, ref.dc);
asserts.isequal(m.dc{20}, 'text');
asserts.isequal(class(m.mi), 'missing');
asserts.isequal(size(m.mi), [3 2]);
asserts.isequal(m.p, ref.p);
asserts.isequal([m.p.X, m.p.Y], [3 4]);
asserts.isequal(size(m.pa), [1 2]);
asserts.isequal([m.pa.X], [1 5]);
asserts.isequal(class(m.h), 'Nh5Counter');
asserts.isequal(m.h.Count, 7);
% save and load hooks of the class run as with loadnh5
asserts.isequal(m.k.Value, ref.k.Value);
asserts.isequal(m.k.Label, 'nh5_saved_loaded');
%=============================================================================
% size, who and whos describe them without loading
asserts.isequal(size(m, 'd'), [1 1]);
asserts.isequal(size(m, 'mi'), [3 2]);
asserts.isequal(size(m, 'pa'), [1 2]);
w = whos(m, 'd', 'mi', 'p');
asserts.isequal({w.class}, {'dictionary', 'missing', 'Nh5Point'});
txt = evalc('disp(m)');
asserts.istrue(contains(txt, 'd: [1x1 dictionary]'));
asserts.istrue(contains(txt, 'p: [1x1 Nh5Point]'));
%=============================================================================
% missing is a builtin array: its blocks come from the whole variable
asserts.isequal(class(m.mi(2:3, 1)), 'missing');
asserts.isequal(size(m.mi(2:3, 1)), [2 1]);
%=============================================================================
% objects are read and written whole only
asserts.checkerror('m.d(1, 1)', '''d'' (dictionary) can only be read or written whole.');
asserts.checkerror('m.p(1, 1)', '''p'' (Nh5Point) can only be read or written whole.');
asserts.checkerror('m.pa(1, 2)', '''pa'' (Nh5Point) can only be read or written whole.');
asserts.checkerror('m.h(1, 1)', '''h'' (Nh5Counter) can only be read or written whole.');
asserts.checkerror('m.d(1, 1) = 5;', '''d'' (dictionary) can only be read or written whole.');
asserts.checkerror('m.p(1, 1) = 5;', '''p'' (Nh5Point) can only be read or written whole.');
asserts.checkerror('m.mi(1, 1) = 5;', ...
  'The value cannot be converted to the class of the variable in the file.');
%=============================================================================
% whole writes replace them, the other variables are kept
d2 = insert(d, "z", 26);
m.d = d2;
m.p = Nh5Point(-1, -2);
m.mi = repmat(missing, 1, 4);
c2 = Nh5Counter();
c2.Count = 42;
m.h = c2;
m.newd = dictionary(1, "one");
after = loadnh5('objects.nh5');
asserts.isequal(after.d, d2);
asserts.isequal(lookup(after.d, "z"), 26);
asserts.isequal([after.p.X, after.p.Y], [-1 -2]);
asserts.isequal(size(after.mi), [1 4]);
asserts.isequal(after.h.Count, 42);
asserts.isequal(lookup(after.newd, 1), "one");
asserts.isequal(after.dc, ref.dc);
asserts.isequal([after.pa.X], [1 5]);
%=============================================================================
% a handle read from the file is a new object, not the saved one
c2.Count = 0;
asserts.isequal(m.h.Count, 42);
%=============================================================================
rmpath(folder);
%=============================================================================
