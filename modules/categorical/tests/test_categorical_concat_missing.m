%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% missing concatenated with a categorical array is <undefined> and adds no category
C = [categorical({'a'}) missing];
assert_isequal(size(C), [1 2]);
assert_isequal(isundefined(C), [false true]);
assert_isequal(categories(C), {'a'});
C = [missing; categorical({'a'})];
assert_isequal(size(C), [2 1]);
assert_isequal(isundefined(C), [true; false]);
assert_isequal(categories(C), {'a'});
C = cat(1, missing, categorical({'b'}));
assert_isequal(isundefined(C), [true; false]);
assert_isequal(categories(C), {'b'});
C = [missing missing categorical({'a'})];
assert_isequal(isundefined(C), [true true false]);
assert_isequal(categories(C), {'a'});
%=============================================================================
