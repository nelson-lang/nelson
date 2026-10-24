%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% This is the hist() that answers when the graphics module is not loaded, and it
% used to read a style standing in the bin slot as a bin specification, in two
% different ways: a single character passed isscalar() and counted as a number
% of bins, while a longer one passed isreal() and counted as bin centres sitting
% at the codes of its characters -- so hist(Y, 'ro') laid its bins out at 111
% and 114 and answered counts, silently, instead of refusing the call.
%
% The bin specification has to be numeric. The graphics hist() refuses the same
% thing the same way: the two count their bins alike, so they have to turn the
% same calls away.
%=============================================================================
Y = [1 2 2 3 3 3];
M = [1 2; 2 3; 3 3];
BIN_ERROR_ID = 'Nelson:validators:mustBeNumeric';
BIN_ERROR_MESSAGE = sprintf(_('%s must be numeric.'), 'Second argument');
%=============================================================================
rejected = {...
  'counts = hist(Y, ''r'')', ...
  'counts = hist(Y, ''ro'')', ...
  'counts = hist(Y, ''FaceColor'')', ...
  'counts = hist(Y, "r")', ...
  'counts = hist(Y, '''')', ...
  'counts = hist(M, ''r'')', ...
  'counts = hist(Y, {3})', ...
  'counts = hist(Y, struct(''a'', 1))'};
for k = 1:numel(rejected)
  assert_checkerror(rejected{k}, BIN_ERROR_MESSAGE, BIN_ERROR_ID);
end
%=============================================================================
% Asking for the drawing this version cannot do is refused on the bins first:
% the call is turned away before anything is said about a missing module.
assert_checkerror('hist(Y, ''r'')', BIN_ERROR_MESSAGE, BIN_ERROR_ID);
%=============================================================================
% Every numeric form the bins already took is still taken, and every value of
% the sample still lands in a bin.
assert_isequal(sum(hist(Y, 3)), numel(Y));
assert_isequal(sum(hist(Y, [1 2 3])), numel(Y));
countsM = hist(M, 3);
assert_isequal(sum(countsM(:)), numel(M));
%=============================================================================
% A single-precision bin count counts as the number it stands for.
assert_isequal(hist(Y, single(3)), hist(Y, 3));
%=============================================================================
% A logical bin specification counts as the numbers it stands for.
assert_isequal(hist(Y, logical([0 1 1])), hist(Y, [0 1 1]));
%=============================================================================
% An EMPTY bin specification is numeric, and stays accepted: it is the empty
% CHAR above that has to be refused, not emptiness.
assert_isequal(hist(Y, []), numel(Y));
%=============================================================================
