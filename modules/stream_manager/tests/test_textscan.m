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
asserts.isequal(nargin('textscan'), -1);
asserts.isequal(nargout('textscan'), 2);
%=============================================================================
% Simple integer column (int32).
C = textscan('1 2 3', '%d');
asserts.isequal(class(C), 'cell');
asserts.isequal(size(C), [1 1]);
asserts.isequal(C{1}, int32([1; 2; 3]));
%=============================================================================
% String column with an explicit delimiter.
C = textscan('a,b,c', '%s', 'Delimiter', ',');
asserts.isequal(C{1}, {'a'; 'b'; 'c'});
%=============================================================================
% Two floating point columns separated by a literal.
C = textscan('1.5,2.5', '%f,%f');
asserts.isequal(size(C), [1 2]);
asserts.isequal(C{1}, 1.5);
asserts.isequal(C{2}, 2.5);
%=============================================================================
% Character set scanning.
C = textscan('name:42', '%[^:]:%d');
asserts.isequal(C{1}, {'name'});
asserts.isequal(C{2}, int32(42));
%=============================================================================
% %c reads one character per cycle.
C = textscan('abc', '%c');
asserts.isequal(C{1}, ['a'; 'b'; 'c']);
%=============================================================================
% Format is cycled over the whole input.
C = textscan('1 2 3 4 5 6', '%d %d');
asserts.isequal(C{1}, int32([1; 3; 5]));
asserts.isequal(C{2}, int32([2; 4; 6]));
%=============================================================================
% Numbers separated by newlines.
C = textscan(sprintf('1\n2\n3'), '%f');
asserts.isequal(C{1}, [1; 2; 3]);
%=============================================================================
% Integer conversion rounds the parsed value.
C = textscan('12.5', '%d');
asserts.isequal(C{1}, int32(13));
C = textscan('1.9 2.1', '%u');
asserts.isequal(C{1}, uint32([2; 2]));
%=============================================================================
% Signed integers.
C = textscan('-5 +3 10', '%d');
asserts.isequal(C{1}, int32([-5; 3; 10]));
%=============================================================================
% Scientific notation and special values.
C = textscan('3.14e2 1.5', '%f');
asserts.isequal(C{1}, [314; 1.5]);
C = textscan('1 Inf -Inf NaN 2', '%f');
asserts.isequal(C{1}, [1; Inf; -Inf; NaN; 2]);
%=============================================================================
% Field width.
C = textscan('abc def', '%3s');
asserts.isequal(C{1}, {'abc'; 'def'});
C = textscan('ab12cd34', '%2c%2d');
asserts.isequal(C{1}, ['ab'; 'cd']);
asserts.isequal(C{2}, int32([12; 34]));
%=============================================================================
% Quoted strings.
C = textscan('"hello" "wor""ld"', '%q');
asserts.isequal(C{1}, {'hello'; 'wor"ld'});
%=============================================================================
% Character sets match literally: '-' is an ordinary member, not a range.
C = textscan('abcdef', '%[a-c]');
asserts.isequal(C{1}, {'a'});
C = textscan('ab-12', '%[a-z]-%d');
asserts.isequal(C{1}, {'a'});
asserts.isequal(size(C{2}), [0 1]);
%=============================================================================
% Assignment suppression.
C = textscan('1 2 3 4', '%d %*d');
asserts.isequal(numel(C), 1);
asserts.isequal(C{1}, int32([1; 3]));
%=============================================================================
% EmptyValue for missing numeric fields.
C = textscan('1;2;;4', '%f', 'Delimiter', ';', 'EmptyValue', -1);
asserts.isequal(C{1}, [1; 2; -1; 4]);
%=============================================================================
% Empty string fields with a delimiter.
C = textscan('a,b,,d', '%s', 'Delimiter', ',');
asserts.isequal(C{1}, {'a'; 'b'; ''; 'd'});
%=============================================================================
% HeaderLines skips leading lines.
C = textscan(sprintf('h1\nh2\n1 2\n3 4'), '%f %f', 'HeaderLines', 2);
asserts.isequal(C{1}, [1; 3]);
asserts.isequal(C{2}, [2; 4]);
%=============================================================================
% CollectOutput merges consecutive columns of the same class.
C = textscan('1,2,3,4', '%d%d', 'Delimiter', ',', 'CollectOutput', true);
asserts.isequal(numel(C), 1);
asserts.isequal(C{1}, int32([1 2; 3 4]));
%=============================================================================
C = textscan('1,2,3,4,5,6', '%d %d %d', 'Delimiter', ',', 'CollectOutput', true);
asserts.isequal(C{1}, int32([1 2 3; 4 5 6]));
%=============================================================================
% CollectOutput keeps columns of different classes separated.
C = textscan('1 a 2 b', '%d %s', 'CollectOutput', true);
asserts.isequal(numel(C), 2);
asserts.isequal(C{1}, int32([1; 2]));
asserts.isequal(C{2}, {'a'; 'b'});
%=============================================================================
% MultipleDelimsAsOne.
C = textscan('1,,,2,,3', '%d', 'Delimiter', ',', 'MultipleDelimsAsOne', true);
asserts.isequal(C{1}, int32([1; 2; 3]));
%=============================================================================
% CommentStyle removes the remainder of a line.
C = textscan(sprintf('1 2 # note\n3 4'), '%f', 'CommentStyle', '#');
asserts.isequal(C{1}, [1; 2; 3; 4]);
%=============================================================================
% Empty result columns keep the expected class.
C = textscan('hello', '%d');
asserts.isequal(class(C{1}), 'int32');
asserts.isequal(size(C{1}), [0 1]);
C = textscan('', '%f');
asserts.isequal(size(C{1}), [0 1]);
%=============================================================================
% Second output returns the position where scanning stopped.
[C, position] = textscan('1 2 3', '%d');
asserts.isequal(position, 5);
[C, position] = textscan('1 2 3 x', '%d');
asserts.isequal(position, 6);
%=============================================================================
% Reading from an open file identifier.
filename = [tempname(), '.txt'];
fw = fopen(filename, 'w');
fprintf(fw, '# header\n1 2\n3 4\n5 6\n');
fclose(fw);
fid = fopen(filename, 'r');
C = textscan(fid, '%f %f', 'CommentStyle', '#');
fclose(fid);
asserts.isequal(C{1}, [1; 3; 5]);
asserts.isequal(C{2}, [2; 4; 6]);
rmfile(filename);
%=============================================================================
% String scalar input.
C = textscan("10 20 30", '%d');
asserts.isequal(C{1}, int32([10; 20; 30]));
%=============================================================================
% Error cases.
asserts.checkerror('textscan(''1 2'')', _('Not enough input arguments.'));
asserts.checkerror('textscan(''1 2'', ''literal only'')', _('The format must contain at least one conversion specifier.'));
asserts.checkerror('textscan(''1 2'', ''%d'', ''UnknownOption'', 1)', sprintf(_('Unknown option ''%s''.'), 'UnknownOption'));
%=============================================================================
