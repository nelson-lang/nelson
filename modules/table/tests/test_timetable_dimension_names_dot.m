%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Dot indexing follows DimensionNames once the time dimension is renamed.
TT = timetable(seconds([2; 1; 2]), [4; 4; 4]);
TT.Properties.DimensionNames{1} = 'Date';
TT.Properties.VariableNames = {'Time'};
asserts.isequal(TT.Properties.DimensionNames, {'Date', 'Variables'});
asserts.isequal(TT.Time, [4; 4; 4]);
asserts.isequal(TT.Date, seconds([2; 1; 2]));
asserts.isequal(TT.Properties.RowTimes, seconds([2; 1; 2]));
%=============================================================================
% Dot assignment follows DimensionNames too.
TT.Time = [5; 6; 7];
asserts.isequal(TT.Time, [5; 6; 7]);
asserts.isequal(TT.Date, seconds([2; 1; 2]));
TT.Date = seconds([3; 4; 5]);
asserts.isequal(TT.Properties.RowTimes, seconds([3; 4; 5]));
asserts.isequal(TT.Time, [5; 6; 7]);
%=============================================================================
% The old default dimension name is no longer a row-times alias.
TT = timetable(seconds([1; 2]), [4; 5]);
TT.Properties.DimensionNames{1} = 'Date';
asserts.checkerror('TT.Time', sprintf(_('Unrecognized table variable name ''%s''.'), 'Time'));
%=============================================================================
% Renamed second dimension.
TT = timetable(seconds([1; 2]), [4; 5], [6; 7]);
TT.Properties.DimensionNames{2} = 'Data';
asserts.isequal(TT.Data, [4, 6; 5, 7]);
%=============================================================================
msg = sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Time');
asserts.checkerror('timetable(seconds([2; 1; 2]), [4; 4; 4], ''VariableNames'', {''Time''})', msg);
asserts.checkerror('timetable([4; 4; 4], ''RowTimes'', seconds([2; 1; 2]), ''VariableNames'', {''Time''})', msg);
try
  timetable(seconds([2; 1; 2]), [4; 4; 4], 'VariableNames', {'Time'});
  asserts.istrue(false);
catch ex
  asserts.isequal(ex.identifier, 'Nelson:timetable:duplicateDimensionVariableNames');
end
%=============================================================================
msg = sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Variables');
asserts.checkerror('timetable(seconds([1; 2]), [4; 5], ''VariableNames'', {''Variables''})', msg);
%=============================================================================
msg = sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Date');
asserts.checkerror('timetable(seconds([1; 2]), [4; 5], ''VariableNames'', {''Date''}, ''DimensionNames'', {''Date'', ''Variables''})', msg);
%=============================================================================
% Later assignments of VariableNames or DimensionNames are checked as well.
TT = timetable(seconds([1; 2]), [4; 5]);
msg = sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Time');
asserts.checkerror('TT.Properties.VariableNames = {''Time''};', msg);
asserts.checkerror('TT.Properties.VariableNames{1} = ''Time'';', msg);
asserts.isequal(TT.Properties.VariableNames, {'Var1'});
msg = sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Var1');
asserts.checkerror('TT.Properties.DimensionNames{1} = ''Var1'';', msg);
asserts.checkerror('TT.Properties.DimensionNames = {''Time'', ''Var1''};', msg);
asserts.isequal(TT.Properties.DimensionNames, {'Time', 'Variables'});
asserts.isequal(TT.Time, seconds([1; 2]));
asserts.isequal(TT.Var1, [4; 5]);
%=============================================================================
