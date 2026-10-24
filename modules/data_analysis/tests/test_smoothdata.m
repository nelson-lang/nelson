%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
skip_testsuite(isempty(which('fft')), 'smoothdata default window heuristic requires fft.');
%=============================================================================
% movmean with an explicit window
assert_isapprox(smoothdata([1 2 10 4 5], 'movmean', 3), ...
  [1.5 4.33333333333333 5.33333333333333 6.33333333333333 4.5], 1e-10);
%=============================================================================
% movmean is the default method (no method given)
assert_isapprox(smoothdata([1 2 10 4 5], 'movmean', 2), ...
  smoothdata([1 2 10 4 5]), 1e-12);
%=============================================================================
% movmedian
assert_isapprox(smoothdata([1 2 10 4 5], 'movmedian', 3), ...
  [1.5 2 4 5 4.5], 1e-10);
%=============================================================================
% gaussian with an odd window
assert_isapprox(smoothdata([1 2 10 4 5], 'gaussian', 3), ...
  [1.19958519865373 3.16464957398946 7.67070085202109 5.16464957398946 4.80041480134627], 1e-10);
%=============================================================================
% gaussian with an even window
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'gaussian', 4), ...
  [1.31405054499181 3.67295985936093 6.52730824290629 5.59060718924216 5.11210668408053 5.02061454758243 3.97310085107074], 1e-10);
%=============================================================================
% gaussian with a two-element [back forward] window
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'gaussian', [1 2]), ...
  [1.22673266806167 3.16679882314899 7.66382948237447 5.16679882314899 4.99485425734491 5.33448595772031 3.5987555959612], 1e-10);
%=============================================================================
% default heuristic window is data dependent
[y, w] = smoothdata([1 2 10 4 5]);
assert_isequal(w, 2);
assert_isapprox(y, [1 1.5 6 7 4.5], 1e-10);
%=============================================================================
% columns of a matrix are smoothed independently
M = [1 2; 2 10; 10 4; 4 5; 5 6];
assert_isapprox(smoothdata(M, 'movmean', 3), ...
  [1.5 6; 4.33333333333333 5.33333333333333; 5.33333333333333 6.33333333333333; 6.33333333333333 5; 4.5 5.5], 1e-10);
assert_isapprox(smoothdata(M, 'gaussian', 3), ...
  [1.19958519865373 3.59668158922986; 3.16464957398946 7.67070085202109; 7.67070085202109 5.16464957398946; 5.16464957398946 5; 4.80041480134627 5.80041480134627], 1e-10);
%=============================================================================
% explicit dimension
assert_isapprox(smoothdata(M, 2, 'movmean', 3), ...
  [1.5 1.5; 6 6; 7 7; 4.5 4.5; 5.5 5.5], 1e-10);
%=============================================================================
% omitnan is the default for the moving methods
assert_isapprox(smoothdata([1 NaN 10 4 5], 'movmean', 3), ...
  [1 5.5 7 6.33333333333333 4.5], 1e-10);
%=============================================================================
% includenan propagates NaN
R = smoothdata([1 NaN 10 4 5], 'movmean', 3, 'includenan');
assert_istrue(all(isnan(R(1:3))));
assert_isapprox(R(4:5), [6.33333333333333 4.5], 1e-10);
%=============================================================================
% gaussian omitnan
assert_isapprox(smoothdata([1 NaN 10 4 5], 'gaussian', 3), ...
  [1 5.5 8.80248880807761 5.16464957398946 4.80041480134627], 1e-10);
%=============================================================================
% lowess
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'lowess', 5), ...
  [0.744894805398465 3.63691165629558 5.99165275459099 6.00417362270451 5 4.53949333981946 3.87562228891785], 1e-10);
%=============================================================================
% loess
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'loess', 5), ...
  [0.276986558773071 4.73100224659106 10 4 5 5.47980909588742 3.13771684594799], 1e-10);
%=============================================================================
% sgolay
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'sgolay', 5), ...
  [0.399999999999999 4.4 6.4 6.4 4.74285714285714 3.97142857142857 4.05714285714285], 1e-10);
%=============================================================================
% sgolay with an explicit degree
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'sgolay', 5, 'Degree', 3), ...
  [0.399999999999997 4.4 6.4 6.4 4.74285714285714 6.17142857142857 2.95714285714285], 1e-8);
%=============================================================================
% SmoothingFactor tunes the default window
[~, w2] = smoothdata([1 2 10 4 5 6 3], 'movmean', 'SmoothingFactor', 0.9);
assert_isequal(w2, 4);
%=============================================================================
% uniformly spaced SamplePoints rescale the window
assert_isapprox(smoothdata([1 2 10 4 5 6 3], 'movmean', 2, 'SamplePoints', (1:7) * 0.5), ...
  [1.5 4.33333333333333 4.25 5.25 6.25 4.5 4.66666666666667], 1e-10);
%=============================================================================
% partial method names are accepted
assert_isapprox(smoothdata([1 2 10 4 5], 'gauss', 3), ...
  smoothdata([1 2 10 4 5], 'gaussian', 3), 1e-12);
%=============================================================================
% a row vector stays a row vector, a column vector stays a column vector
assert_isequal(size(smoothdata([1 2 3 4 5], 'movmean', 3)), [1 5]);
assert_isequal(size(smoothdata([1; 2; 3; 4; 5], 'movmean', 3)), [5 1]);
%=============================================================================
% 'Degree' is rejected for methods other than sgolay
assert_checkerror('smoothdata([1 2 3], ''movmean'', 3, ''Degree'', 2)', ...
  _('''Degree'' is only supported for the ''sgolay'' method.'));
%=============================================================================
% robust local regression is not supported yet
assert_checkerror('smoothdata([1 2 3 4 5], ''rlowess'', 3)', ...
  _('Robust local regression (''rlowess''/''rloess'') is not supported yet.'));
%=============================================================================
