%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% The bin specification must be numeric. A style sitting there used to be read
% as a bin specification all the same, in two different ways: a single
% character counted as a number of bins and broke inside the colon operator,
% while a longer one counted as bin centres and was binned against the codes of
% its characters -- silently, so hist(Y, 'ro') answered counts around 111 and
% 114 instead of refusing the call.
%
% A style is read from the third argument on, never from the second: the bins
% have to be given before anything can be said about how the bars look.
%=============================================================================
Y = [1 2 2 3 3 3];
BIN_ERROR_ID = 'Nelson:validators:mustBeNumeric';
BIN_ERROR_MESSAGE = getString(message('Nelson:validators:mustBeNumeric', 'Second argument'));
%=============================================================================
% Counting form: nothing is drawn, and the bins are refused all the same.
rejected = {...
  'counts = hist(Y, ''r'')', ...
  'counts = hist(Y, ''ro'')', ...
  'counts = hist(Y, "r")', ...
  'counts = hist(Y, '''')', ...
  'counts = hist(Y, ''FaceColor'', [0 1 0])', ...
  'counts = hist([1 2; 2 3; 3 3], ''r'')', ...
  'counts = hist(Y, {3})', ...
  'counts = hist(Y, struct(''a'', 1))'};
for k = 1:numel(rejected)
  assert_checkerror(rejected{k}, BIN_ERROR_MESSAGE, BIN_ERROR_ID);
end
%=============================================================================
% Drawing form: same refusal, whether or not a target was designated.
f = figure('Visible', 'off');
assert_checkerror('hist(Y, ''r'')', BIN_ERROR_MESSAGE, BIN_ERROR_ID);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
assert_checkerror('hist(ax1, Y, ''r'')', BIN_ERROR_MESSAGE, BIN_ERROR_ID);
assert_checkerror('hist(Y, ''r'', ''Parent'', ax1)', BIN_ERROR_MESSAGE, BIN_ERROR_ID);
close(f);
%=============================================================================
% Every numeric form the bins already took is still taken.
assert_isequal(hist(Y, 3), [1 2 3]);
assert_isequal(hist(Y, [1 2 3]), [1 2 3]);
assert_isequal(hist(Y, single(3)), [1 2 3]);
%=============================================================================
% An EMPTY bin specification is numeric, and stays accepted: it is the empty
% CHAR above that has to be refused, not emptiness.
assert_isequal(hist(Y, []), numel(Y));
%=============================================================================
% A logical bin specification counts as the numbers it stands for.
assert_isequal(hist(Y, logical([1 0 1])), hist(Y, [0 1 1]));
%=============================================================================
% A style is still honoured from the third argument on.
f = figure('Visible', 'off');
hist(Y, 3, 'r');
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.FaceColor, [1 0 0], ...
  'A style following the bins must still paint the bars.');
close(f);
%=============================================================================
f = figure('Visible', 'off');
hist(Y, [1 2 3], 'g');
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.FaceColor, [0 1 0], ...
  'A style following explicit bin centres must still paint the bars.');
close(f);
%=============================================================================
% The counts never depend on the style that follows the bins.
assert_isequal(hist(Y, 3, 'r'), hist(Y, 3));
%=============================================================================
