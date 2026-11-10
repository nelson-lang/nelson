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
savemat('existing.mat', 'x', '-v7.3');
%=============================================================================
% existing file: read-only, the extension is optional
m = matfile('existing');
asserts.isequal(class(m), 'nelson.io.MatFile');
asserts.istrue(isa(m, 'handle'));
asserts.isequal(class(m.Properties), 'nelson.io.internal.FileProperties');
asserts.istrue(isa(m, 'nelson.io.internal.PartialFile'));
asserts.isequal(m.Properties.Source, fullfile(folder, 'existing.mat'));
asserts.isequal(m.Properties.Writable, false);
asserts.isequal(m.x, x);
%=============================================================================
% new file: writable, created on the first assignment
n = matfile("created.mat");
asserts.isequal(n.Properties.Writable, true);
asserts.isfalse(isfile('created.mat'));
n.a = 1;
asserts.istrue(isfile('created.mat'));
asserts.isequal(n.a, 1);
%=============================================================================
m = matfile('existing.mat', 'Writable', true);
asserts.isequal(m.Properties.Writable, true);
m.Properties.Writable = false;
asserts.isequal(m.Properties.Writable, false);
n = matfile('created.mat', 'Writable', false);
asserts.isequal(n.Properties.Writable, false);
%=============================================================================
asserts.checkerror('matfile()', _('Not enough input arguments.'));
asserts.checkerror('matfile(3)', 'File name: string scalar or character vector expected.');
asserts.checkerror('matfile(''existing.mat'', ''Writable'', ''yes'')', ...
  'Writable must be true or false.');
asserts.checkerror('matfile(''existing.mat'', ''Foo'', 1)', ...
  'Unknown option ''Foo'': only ''Writable'' is supported.');
%=============================================================================
m = matfile('existing.mat');
asserts.checkerror('m.Properties.Writable = 1;', ...
  'Writable must be true or false.');
asserts.checkerror('m.Properties(1)', ...
  'Use m.Properties.Source or m.Properties.Writable.');
asserts.checkerror('m(1)', ...
  'Use m.name or m.name(i1, ..., iN) to access a variable.');
asserts.checkerror('[m, m]', 'nelson.io.MatFile objects cannot be concatenated.');
asserts.checkerror('[m; m]', 'nelson.io.MatFile objects cannot be concatenated.');
asserts.checkerror('cat(3, m, m)', 'nelson.io.MatFile objects cannot be concatenated.');
m.Properties.Writable = true;
asserts.checkerror('m.Properties = 1;', ...
  '''Properties'' is reserved and cannot be used as a variable name.');
%=============================================================================
% display lists the description then each variable
m = matfile('existing.mat');
txt = evalc('disp(m)');
asserts.istrue(contains(txt, 'nelson.io.MatFile'));
asserts.istrue(contains(txt, '  Properties:'));
asserts.istrue(contains(txt, ['    Properties.Source: ''', fullfile(folder, 'existing.mat'), '''']));
asserts.istrue(contains(txt, '  Properties.Writable: false'));
asserts.istrue(contains(txt, '                    x: [4x4 double]'));
%=============================================================================
