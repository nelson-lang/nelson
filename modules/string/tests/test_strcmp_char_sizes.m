%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% strcmp and strcmpi of two char arrays need the same size and characters,
% empty ones included; a string or a cell compares the text alone.
%=============================================================================
asserts.istrue(strcmp('', ''));
asserts.isfalse(strcmp('', char(zeros(1, 0))));
asserts.isfalse(strcmpi('', char(zeros(1, 0))));
asserts.istrue(strcmp(char(zeros(1, 0)), char(zeros(1, 0))));
asserts.isfalse(strcmp(char(zeros(0, 3)), ''));
asserts.istrue(strcmp(char(zeros(0, 3)), char(zeros(0, 3))));
asserts.isfalse(strcmp(char(zeros(0, 3)), char(zeros(3, 0))));
%=============================================================================
asserts.istrue(strcmp(['ab'; 'cd'], ['ab'; 'cd']));
asserts.isfalse(strcmp(['ab'; 'cd'], ['ac'; 'bd']));
asserts.isfalse(strcmp(['ab'; 'cd'], 'abcd'));
asserts.isfalse(strcmp('abcd', ['ab'; 'cd']));
asserts.isfalse(strcmp(['AB'; 'cd'], ['ab'; 'CD']));
asserts.istrue(strcmpi(['AB'; 'cd'], ['ab'; 'CD']));
asserts.isfalse(strcmp(['a', char(0), 'b'; 'cdf'], ['a', char(0), 'c'; 'cdf']));
%=============================================================================
asserts.istrue(strcmp('', ""));
asserts.istrue(strcmp(char(zeros(1, 0)), ""));
asserts.isequal(strcmp({'', 'a', char(zeros(1, 0))}, ''), [true, false, true]);
asserts.istrue(strcmp({''}, char(zeros(1, 0))));
%=============================================================================
