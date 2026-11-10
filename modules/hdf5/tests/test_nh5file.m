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
%=============================================================================
x = magic(4);
savenh5('existing.nh5', 'x');
%=============================================================================
% existing file: read-only, the extension is optional
m = nh5file('existing');
asserts.isequal(class(m), 'nelson.io.Nh5File');
asserts.istrue(isa(m, 'handle'));
asserts.isequal(class(m.Properties), 'nelson.io.internal.FileProperties');
asserts.istrue(isa(m, 'nelson.io.internal.PartialFile'));
asserts.isequal(m.Properties.Source, fullfile(folder, 'existing.nh5'));
asserts.isequal(m.Properties.Writable, false);
asserts.isequal(m.x, x);
%=============================================================================
% new file: writable, created on the first assignment
n = nh5file("created.nh5");
asserts.isequal(n.Properties.Writable, true);
asserts.isfalse(isfile('created.nh5'));
n.a = 1;
asserts.istrue(isfile('created.nh5'));
asserts.isequal(n.a, 1);
%=============================================================================
m = nh5file('existing.nh5', 'Writable', true);
asserts.isequal(m.Properties.Writable, true);
m.Properties.Writable = false;
asserts.isequal(m.Properties.Writable, false);
n = nh5file('created.nh5', 'Writable', false);
asserts.isequal(n.Properties.Writable, false);
%=============================================================================
asserts.checkerror('nh5file()', _('Not enough input arguments.'));
asserts.checkerror('nh5file(3)', 'File name: string scalar or character vector expected.');
asserts.checkerror('nh5file(''existing.nh5'', ''Writable'', ''yes'')', ...
  'Writable must be true or false.');
asserts.checkerror('nh5file(''existing.nh5'', ''Foo'', 1)', ...
  'Unknown option ''Foo'': only ''Writable'' is supported.');
%=============================================================================
m = nh5file('existing.nh5');
asserts.checkerror('m.Properties.Writable = 1;', ...
  'Writable must be true or false.');
asserts.checkerror('m.Properties(1)', ...
  'Use m.Properties.Source or m.Properties.Writable.');
asserts.checkerror('m(1)', ...
  'Use m.name or m.name(i1, ..., iN) to access a variable.');
asserts.checkerror('[m, m]', 'nelson.io.Nh5File objects cannot be concatenated.');
asserts.checkerror('[m; m]', 'nelson.io.Nh5File objects cannot be concatenated.');
asserts.checkerror('cat(3, m, m)', 'nelson.io.Nh5File objects cannot be concatenated.');
m.Properties.Writable = true;
asserts.checkerror('m.Properties = 1;', ...
  '''Properties'' is reserved and cannot be used as a variable name.');
%=============================================================================
% display lists the description then each variable
m = nh5file('existing.nh5');
txt = evalc('disp(m)');
asserts.istrue(contains(txt, 'nelson.io.Nh5File'));
asserts.istrue(contains(txt, '  Properties:'));
asserts.istrue(contains(txt, ['    Properties.Source: ''', fullfile(folder, 'existing.nh5'), '''']));
asserts.istrue(contains(txt, '  Properties.Writable: false'));
asserts.istrue(contains(txt, '                    x: [4x4 double]'));
%=============================================================================
