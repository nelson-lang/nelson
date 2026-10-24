%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a count below one compares nothing, an infinite count the whole strings,
% a fractional count is rounded down
asserts.istrue(strncmp('abc', 'abd', 0));
asserts.istrue(strncmp('abc', 'abd', -1));
asserts.istrue(strncmp('abc', 'abd', 2.5));
asserts.isfalse(strncmp('abc', 'abd', Inf));
asserts.istrue(strncmp('abc', 'abc', Inf));
asserts.istrue(strncmp('', '', 0));
asserts.isequal(strncmp(["abc", "abd"], "ab", 2), [true true]);
asserts.istrue(strncmp('abc', 'abd', int8(2)));
asserts.checkerror('R = strncmp(''abc'', ''abd'', ''2'');', 'Third argument must be a numeric scalar.', 'Nelson:string:strncmp:invalidCount');
asserts.checkerror('R = strncmp(''abc'', ''abd'', true);', 'Third argument must be a numeric scalar.', 'Nelson:string:strncmp:invalidCount');
asserts.checkerror('R = strncmp(''abc'', ''abd'', NaN);', 'Third argument must be a numeric scalar.', 'Nelson:string:strncmp:invalidCount');
%=============================================================================
% case folding follows Unicode
asserts.istrue(strcmpi(char(201), char(233)));
asserts.istrue(strncmpi(['AB', char(201)], ['ab', char(233)], 3));
%=============================================================================
