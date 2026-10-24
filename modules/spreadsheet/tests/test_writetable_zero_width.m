%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A table without variables writes no rows, not even its row names.
T = table('Size', [2 0], 'VariableTypes', {});
T.Properties.RowNames = {'r1', 'r2'};
filename = [tempname(), '.csv'];
writetable(T, filename, 'WriteRowNames', true);
R = fileread(filename);
asserts.isequal(strtrim(R), 'Row');
%=============================================================================
filename = [tempname(), '.csv'];
writetable(T, filename);
R = fileread(filename);
asserts.isequal(strtrim(R), '');
%=============================================================================
filename = [tempname(), '.xml'];
writetable(T, filename, 'WriteRowNames', true);
R = fileread(filename);
asserts.isfalse(contains(R, 'r1'));
asserts.isfalse(contains(R, '<row'));
%=============================================================================
T = table([1; 2]);
T.Properties.RowNames = {'r1', 'r2'};
filename = [tempname(), '.csv'];
writetable(T, filename, 'WriteRowNames', true);
R = fileread(filename);
asserts.isequal(strsplit(strtrim(R), newline), {'Row,Var1', 'r1,1', 'r2,2'});
%=============================================================================
% A table without rows or without variables is an empty root element.
header = ['<?xml version="1.0" encoding="UTF-8"?>', newline];
T = table('Size', [2 0], 'VariableTypes', {});
T.Properties.RowNames = {'r1', 'r2'};
filename = [tempname(), '.xml'];
writetable(T, filename, 'WriteRowNames', true, 'TableNodeName', 'root');
R = strrep(fileread(filename), char(13), '');
asserts.isequal(R, [header, '<root/>', newline]);
%=============================================================================
T = table('Size', [0 2], 'VariableTypes', {'double', 'double'});
filename = [tempname(), '.xml'];
writetable(T, filename);
R = strrep(fileread(filename), char(13), '');
asserts.isequal(R, [header, '<table/>', newline]);
%=============================================================================
filename = [tempname(), '.xml'];
writetable(table(), filename);
R = strrep(fileread(filename), char(13), '');
asserts.isequal(R, [header, '<table/>', newline]);
%=============================================================================
