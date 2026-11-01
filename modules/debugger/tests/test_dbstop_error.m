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
% dbstop if error / dbstop if caught error: dbstatus structure and text.
%=============================================================================
addpath([modulepath('debugger', 'tests'), '/cases/']);
dbclear all
%=============================================================================
dbstop if error
b = dbstatus();
asserts.isequal(size(b), [1 1]);
asserts.isequal(b.name, '');
asserts.isequal(b.file, '');
asserts.istrue(isempty(b.line));
asserts.istrue(isempty(b.anonymous));
asserts.istrue(iscell(b.expression) && isempty(b.expression));
asserts.isequal(b.cond, 'error');
asserts.isequal(b.identifier, {'all'});
txt = evalc('dbstatus');
asserts.istrue(contains(txt, 'Stop if error.'));
asserts.isfalse(contains(txt, 'Stop if caught error.'));
%=============================================================================
dbstop if caught error
b = dbstatus();
asserts.isequal(size(b), [2 1]);
asserts.isequal(b(1).cond, 'error');
asserts.isequal(b(2).cond, 'caught error');
asserts.isequal(b(2).identifier, {'all'});
txt = evalc('dbstatus');
asserts.istrue(contains(txt, 'Stop if error.'));
asserts.istrue(contains(txt, 'Stop if caught error.'));
%=============================================================================
% line breakpoints come first, then a blank line, then the error entries
dbstop in for_cli at 12
txt = evalc('dbstatus');
expected = ['Breakpoint for for_cli is on line 12.', char(10), char(10), 'Stop if error.', char(10), 'Stop if caught error.', char(10)];
asserts.isequal(txt, expected);
b = dbstatus();
asserts.isequal(size(b), [3 1]);
asserts.isequal(b(1).name, 'for_cli');
asserts.isequal(b(1).line, 12);
asserts.isequal(b(1).expression, {''});
asserts.isequal(b(1).cond, '');
asserts.isequal(b(2).cond, 'error');
asserts.isequal(b(3).cond, 'caught error');
% a file filter leaves the error entries out
b = dbstatus('for_cli');
asserts.isequal(size(b), [1 1]);
asserts.isequal(b.name, 'for_cli');
%=============================================================================
% dbclear if removes one condition at a time, dbclear all removes everything
dbclear if error
b = dbstatus();
asserts.isequal(size(b), [2 1]);
asserts.isequal(b(2).cond, 'caught error');
dbclear if caught error
b = dbstatus();
asserts.isequal(size(b), [1 1]);
asserts.isequal(b.name, 'for_cli');
dbstop if error
dbclear all
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% round trip through dbstop(b) restores the error breakpoints
dbstop in for_cli at 12
dbstop if error
dbstop if caught error
b = dbstatus();
dbclear all
asserts.istrue(isempty(dbstatus()));
dbstop(b)
b2 = dbstatus();
asserts.isequal(size(b2), [3 1]);
asserts.isequal(b2(1).line, 12);
asserts.isequal(b2(2).cond, 'error');
asserts.isequal(b2(3).cond, 'caught error');
dbclear all
%=============================================================================
% unsupported conditions and invalid structures
asserts.checkerror('dbstop if warning', _('Unsupported dbstop condition ''warning''. Supported conditions are ''error'' and ''caught error''.'), 'Nelson:dbstop:unsupportedCondition');
asserts.checkerror('dbstop if naninf', _('Unsupported dbstop condition ''naninf''. Supported conditions are ''error'' and ''caught error''.'), 'Nelson:dbstop:unsupportedCondition');
legacy = struct('name', 'for_cli', 'file', which('for_cli'), 'line', 12);
asserts.checkerror('dbstop(legacy)', _('Invalid structure. Field ''anonymous'' is missing.'), 'Nelson:dbstop:invalidStructure');
asserts.istrue(isempty(dbstatus()));
%=============================================================================
