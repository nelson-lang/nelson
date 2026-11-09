%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% dbstack(n) leaves out the n innermost frames, in its output and its display,
% also inside evalc; the current frame is marked; no blank line ends it.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'dbo.m'), ['function out = dbo()', LF, ...
  '  out = dbo_inner();', LF, ...
  'end', LF, ...
  'function out = dbo_inner()', LF, ...
  '  out.names0 = frame_names(dbstack());', LF, ...
  '  out.names1 = frame_names(dbstack(1));', LF, ...
  '  out.names2 = frame_names(dbstack(2));', LF, ...
  '  out.display0 = evalc(''dbstack'');', LF, ...
  '  out.display1 = evalc(''dbstack(1)'');', LF, ...
  '  [~, out.index] = dbstack();', LF, ...
  'end', LF, ...
  'function names = frame_names(st)', LF, ...
  '  names = {st.name};', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
%=============================================================================
r = dbo();
asserts.isequal(r.names0(1:2), {'dbo_inner', 'dbo'});
asserts.isequal(r.names1{1}, 'dbo');
asserts.isequal(numel(r.names1), numel(r.names0) - 1);
asserts.isequal(numel(r.names2), numel(r.names0) - 2);
asserts.isfalse(any(ismember(r.names0, {'dbstack', 'evalc'})));
asserts.isequal(r.index, 1);
%=============================================================================
asserts.istrue(startsWith(r.display0, ['> In dbo>dbo_inner (line 8)', LF, 'In dbo (line 2)']));
asserts.istrue(startsWith(r.display1, 'In dbo (line 2)'));
asserts.isfalse(contains(r.display1, 'dbo_inner'));
asserts.isfalse(endsWith(r.display0, [LF, LF]));
asserts.isfalse(contains(r.display0, ' function '));
%=============================================================================
