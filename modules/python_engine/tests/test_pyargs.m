%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
C = pyargs('A', 1, 'B', 2);
asserts.isequal(class(C), 'pyargs');
asserts.istrue(isa(C, 'pyargs'));
asserts.istrue(isobject(C));
asserts.isequal(size(C), [1 1]);
asserts.isequal(size(C, 1), 1);
asserts.isequal(size(C, 2), 1);
asserts.isequal(numel(C), 1);
asserts.isequal(length(C), 1);
asserts.isequal(fieldnames(C), cell(0, 1));
%=============================================================================
C = pyargs();
asserts.isequal(class(C), 'pyargs');
asserts.isequal(size(C), [1 1]);
asserts.isequal(numel(C), 1);
asserts.isequal(fieldnames(C), cell(0, 1));
%=============================================================================
asserts.checkerror('pyargs(''A'')', _('Name-value arguments expected.'));
asserts.checkerror('pyargs(1, 2)', _('Field names must be string scalars or character vectors.'));
%=============================================================================
C = pyargs('A', 1, 'B', 2);
asserts.checkerror('C.A', _('subsref not allowed for class ''pyargs''.'));
asserts.checkerror('C.A = 3', _('subsasgn not allowed for class ''pyargs''.'));
asserts.checkerror('[C C]', _('horzcat not allowed for class ''pyargs''.'));
asserts.checkerror('[C; C]', _('vertcat not allowed for class ''pyargs''.'));
%=============================================================================
R = pyrun("R = isinstance(A, dict) and A['A'] == 1 and A['B'] == 2", "R", "A", C);
asserts.istrue(R);
%=============================================================================
