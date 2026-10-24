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
% mustBeA accepts a value of the class, of a subclass or of a category.
%=============================================================================
T = table([1; 2], [3; 4]);
TT = timetable(seconds([1; 2]), [1; 2]);
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
%=============================================================================
mustBeA(ET, 'timetable')
mustBeA(ET, 'tabular')
mustBeA(ET, {'table', 'timetable'})
mustBeA(TT, 'tabular')
mustBeA(T, 'tabular')
mustBeA(TT, ["table", "timetable"])
%=============================================================================
mustBeA(1, 'numeric')
mustBeA(int8(1), 'numeric')
mustBeA(1, 'float')
mustBeA(single(1), 'float')
mustBeA(1i, 'float')
mustBeA(int8(1), 'integer')
mustBeA(uint64(1), {'char', 'integer'})
mustBeA(1, ["char", "numeric"])
%=============================================================================
asserts.checkerror('mustBeA(TT, ''table'')', 'Value must have one of these types: ''table''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(T, ''timetable'')', 'Value must have one of these types: ''timetable''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(true, ''numeric'')', 'Value must have one of these types: ''numeric''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(int8(1), ''float'')', 'Value must have one of these types: ''float''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(1, ''integer'')', 'Value must have one of these types: ''integer''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(1, ''NUMERIC'')', 'Value must have one of these types: ''NUMERIC''.', 'Nelson:validators:mustBeA');
asserts.checkerror('mustBeA(1, {''char'', ''string''}, 2)', sprintf('Invalid input argument at position 2.\nValue must have one of these types: ''char'', or ''string''.'), 'Nelson:validators:mustBeA');
%=============================================================================
