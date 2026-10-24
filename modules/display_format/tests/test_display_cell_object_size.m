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
% an object inside a cell or a struct is summarized with the size its class
% reports (data kept in hidden properties), without brackets in a cell
%=============================================================================
format short
LF = char(10);
X = char(215);
%=============================================================================
C = {duration.empty(0,2), datetime.empty(0,2)};
asserts.isequal(evalc('disp(C)'), ['    {0', X, '2 duration}    {0', X, '2 datetime}', LF, LF]);
%=============================================================================
C = {table.empty, seconds([1;2])};
asserts.isequal(evalc('disp(C)'), ['    {0', X, '0 table}    {2', X, '1 duration}', LF, LF]);
%=============================================================================
C = {categorical({'a';'b'}), calendarDuration([1;2],1,1)};
REF = ['    {2', X, '1 categorical}    {2', X, '1 calendarDuration}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
C = {table(1), categorical(zeros(0,3))};
asserts.isequal(evalc('disp(C)'), ['    {1', X, '1 table}    {0', X, '3 categorical}', LF, LF]);
%=============================================================================
C = {table([1;2]); categorical({'a';'b'})};
REF = ['    {2', X, '1 table      }', LF, ...
       '    {2', X, '1 categorical}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
S = struct();
S.t = table([1;2]);
S.c = categorical({'a';'b'});
REF = ['    t: [2', X, '1 table]', LF, ...
       '    c: [2', X, '1 categorical]', LF, LF];
asserts.isequal(evalc('disp(S)'), REF);
%=============================================================================
% user value and handle classes
directory = tempname();
mkdir(directory);
fid = fopen(fullfile(directory, 'CellSizeValue.m'), 'wt');
fprintf(fid, 'classdef CellSizeValue\n  properties\n    x = 1\n  end\nend\n');
fclose(fid);
fid = fopen(fullfile(directory, 'CellSizeHandle.m'), 'wt');
fprintf(fid, 'classdef CellSizeHandle < handle\n  properties\n    x = 1\n  end\nend\n');
fclose(fid);
addpath(directory);
v = CellSizeValue();
C = {v, [v v], CellSizeHandle()};
REF = ['    {1', X, '1 CellSizeValue}    {1', X, '2 CellSizeValue}    {1', X, '1 CellSizeHandle}', LF, LF];
R = evalc('disp(C)');
clear v C
rmpath(directory);
rmdir(directory, 's');
asserts.isequal(R, REF);
%=============================================================================
