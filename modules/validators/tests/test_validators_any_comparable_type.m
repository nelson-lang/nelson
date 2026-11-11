%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% comparison validators accept any type that implements the operator they use
% (char, duration, datetime, ...), not only real numeric or logical values
mustBePositive('abc');
mustBePositive(seconds([1 2]));
mustBeNonnegative(seconds([0 2]));
mustBeNegative(-seconds(1));
mustBeNonpositive(seconds([0 -1]));
mustBeNonzero(seconds([1 -1]));
mustBeNonzero('a');
mustBeInteger(seconds([1 2]));
mustBeInteger('abc');
t = datetime(2024, 1, [2 3]);
mustBeGreaterThan(t, datetime(2024, 1, 1));
mustBeGreaterThanOrEqual(t, datetime(2024, 1, 2));
mustBeLessThan(t, datetime(2024, 1, 4));
mustBeLessThanOrEqual(t, datetime(2024, 1, 3));
mustBeGreaterThan(seconds(5), seconds(1));
mustBeGreaterThan('b', 'a');
%=============================================================================
% the values are still checked
asserts.checkerror('mustBePositive(seconds([1 -2]))', sprintf(_('%s must be positive.'), 'Value'), ...
  'Nelson:validators:mustBePositive');
asserts.checkerror('mustBeNonnegative(-seconds(1))', sprintf(_('%s must be nonnegative.'), 'Value'), ...
  'Nelson:validators:mustBeNonnegative');
asserts.checkerror('mustBeNegative(seconds(0))', sprintf(_('%s must be negative.'), 'Value'), ...
  'Nelson:validators:mustBeNegative');
asserts.checkerror('mustBeNonpositive(seconds(1))', sprintf(_('%s must be nonpositive.'), 'Value'), ...
  'Nelson:validators:mustBeNonpositive');
asserts.checkerror('mustBeNonzero(seconds([1 0]))', sprintf(_('%s must be nonzero.'), 'Value'), ...
  'Nelson:validators:mustBeNonZero');
asserts.checkerror('mustBeInteger(seconds(1.5))', sprintf(_('%s must be an integer.'), 'Value'), ...
  'Nelson:validators:mustBeInteger');
asserts.checkerror('mustBeGreaterThan(datetime(2024, 1, 1), datetime(2024, 1, 2))', ...
  sprintf(_('Value must exceed %s.'), 'the compared value'), 'Nelson:validators:mustBeGreaterThan');
%=============================================================================
% empty values are ignored
mustBePositive(seconds([]));
mustBeInteger(duration.empty(0, 1));
%=============================================================================
