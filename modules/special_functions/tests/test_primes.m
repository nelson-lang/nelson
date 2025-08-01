%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('primes'), 1);
assert_isequal(nargout('primes'), 1);
%=============================================================================
n = uint16(12);
R = primes(n);
REF = uint16([2    3    5    7   11]);
assert_isequal(R, REF);
%=============================================================================
R = primes(25);
REF = [ 2     3     5     7    11    13    17    19    23];
assert_isequal(R, REF);
%=============================================================================
R = primes(3.3);
REF = [ 2     3];
assert_isequal(R, REF);
%=============================================================================
R = primes(single(3.3));
REF = single([ 2     3]);
assert_isequal(R, REF);
%=============================================================================
R = primes(-1);
REF = zeros(1, 0);
assert_isequal(R, REF);
%=============================================================================
R = primes(single(-1));
REF = zeros(1, 0, 'single');
assert_isequal(R, REF);
%=============================================================================
