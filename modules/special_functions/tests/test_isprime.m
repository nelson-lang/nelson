%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isprime'), 1);
asserts.isequal(nargout('isprime'), 1);
%=============================================================================
asserts.isequal(isprime([2 3 4 5 6 7 8 9 10 11]), logical([1 1 0 1 0 1 0 0 0 1]));
asserts.isequal(isprime([0 1]), logical([0 0]));
asserts.isequal(isprime(97), true);
asserts.isequal(isprime(100), false);
asserts.isequal(isprime(7919), true);
asserts.isequal(isprime([2 4; 17 18]), logical([1 0; 1 0]));
asserts.isequal(sum(isprime(1:100)), 25);
asserts.isequal(class(isprime(uint32([5 6]))), 'logical');
%=============================================================================
asserts.checkerror('isprime(-3)', _('Input must contain nonnegative integers.'), 'Nelson:isprime:nonNonnegativeInteger');
asserts.checkerror('isprime(2.5)', _('Input must contain nonnegative integers.'), 'Nelson:isprime:nonNonnegativeInteger');
%=============================================================================
