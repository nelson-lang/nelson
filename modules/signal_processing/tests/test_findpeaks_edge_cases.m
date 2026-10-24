%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% plateaus: the first sample of a flat top is the peak
x = [0 1 1 1 0 2 2 0 3 0 3 3 3 3 1 5 5 4 4 6 6 6 2 2 2 2 7 0];
[pks, locs, w, p] = findpeaks(x);
asserts.isequal(pks, [1 2 3 3 5 6 7]);
asserts.isequal(locs, [2 6 9 11 16 20 27]);
asserts.isapprox(w, [3 2 1 23 / 6 1.625 3.5 1.2], 1e-12);
asserts.isequal(p, [1 2 3 2 1 4 7]);
%=============================================================================
[pks, locs, w, p] = findpeaks(x, 'MinPeakDistance', 2);
asserts.isequal(pks, [1 2 3 5 6 7]);
asserts.isequal(locs, [2 6 9 16 20 27]);
asserts.isapprox(w, [3 2 1 1.625 3.5 1.2], 1e-12);
asserts.isequal(p, [1 2 3 1 4 7]);
%=============================================================================
[pks, locs, w, p] = findpeaks(x, 'WidthReference', 'halfheight');
asserts.isequal(pks, [1 2 3 3 5 6 7]);
asserts.isequal(locs, [2 6 9 11 16 20 27]);
asserts.isapprox(w, [3 2 1 4.25 7.5 7.25 1.2], 1e-12);
asserts.isequal(p, [1 2 3 2 1 4 7]);
%=============================================================================
% column input gives column outputs
[pks, locs, w, p] = findpeaks(x.');
asserts.isequal(size(pks), [7 1]);
asserts.isequal(size(locs), [7 1]);
asserts.isequal(size(w), [7 1]);
asserts.isequal(size(p), [7 1]);
%=============================================================================
% NaN: a sample next to a NaN is never a peak, a NaN met on the way to the
% width crossing gives a NaN width and the peak is dropped, a NaN met while
% searching the prominence base is ignored
[pks, locs, w, p] = findpeaks([0 4 3 NaN 0 5 0]);
asserts.isequal(pks, 5);
asserts.isequal(locs, 6);
asserts.isequal(w, 1);
asserts.isequal(p, 5);
[pks, locs, w, p] = findpeaks([0 1 0 NaN 0 3 0 2 0]);
asserts.isequal(pks, [1 3 2]);
asserts.isequal(locs, [2 6 8]);
asserts.isequal(w, [1 1 1]);
asserts.isequal(p, [1 3 2]);
[pks, locs, w, p] = findpeaks([1 NaN 3 NaN 2 NaN 5 1]);
asserts.isequal(pks, zeros(1, 0));
asserts.isequal(locs, zeros(1, 0));
asserts.isequal(w, zeros(1, 0));
asserts.isequal(p, zeros(1, 0));
%=============================================================================
% Inf: an infinite peak has an infinite prominence, so its half prominence
% width is undefined and the peak is dropped unless the width is measured at
% half height
[pks, locs, w, p] = findpeaks([NaN Inf 1 3 2]);
asserts.isequal(pks, 3);
asserts.isequal(locs, 4);
asserts.isequal(w, 0.75);
asserts.isequal(p, 1);
[pks, locs, w, p] = findpeaks([1 Inf NaN Inf 2 1]);
asserts.isequal(pks, zeros(1, 0));
asserts.isequal(locs, zeros(1, 0));
[pks, locs, w, p] = findpeaks([1 Inf NaN Inf 2 1], 'WidthReference', 'halfheight');
asserts.isequal(pks, [Inf Inf]);
asserts.isequal(locs, [2 4]);
asserts.isequal(w, [0 0]);
asserts.isequal(p, [Inf Inf]);
[pks, locs, w, p] = findpeaks([1 -Inf 3 -Inf 2 1]);
asserts.isequal(pks, zeros(1, 0));
asserts.isequal(locs, zeros(1, 0));
%=============================================================================
% equal peaks and MinPeakDistance: the first of two equal peaks wins
[pks, locs, w, p] = findpeaks([0 1 0 1 0 1 0]);
asserts.isequal(pks, [1 1 1]);
asserts.isequal(locs, [2 4 6]);
asserts.isequal(w, [1 1 1]);
asserts.isequal(p, [1 1 1]);
[pks, locs, w, p] = findpeaks([0 1 0 1 0 1 0], 'MinPeakDistance', 2);
asserts.isequal(pks, [1 1]);
asserts.isequal(locs, [2 6]);
[pks, locs] = findpeaks([0 1 0 1 0 1 0], 'MinPeakDistance', 2, 'SortStr', 'descend');
asserts.isequal(pks, [1 1]);
asserts.isequal(locs, [2 6]);
[pks, locs] = findpeaks([1 1 1 1]);
asserts.isequal(pks, zeros(1, 0));
asserts.isequal(locs, zeros(1, 0));
%=============================================================================
% location vector: distances and widths are measured in X units
t = [0 1 2 3 4 5 6 7 8];
[pks, locs, w, p] = findpeaks([0 1 0 2 0 3 0 4 0], 2 * t);
asserts.isequal(pks, [1 2 3 4]);
asserts.isequal(locs, [2 6 10 14]);
asserts.isequal(w, [2 2 2 2]);
asserts.isequal(p, [1 2 3 4]);
[pks, locs] = findpeaks([0 1 0 2 0 3 0 4 0], 2 * t, 'MinPeakDistance', 4);
asserts.isequal(pks, [2 4]);
asserts.isequal(locs, [6 14]);
%=============================================================================
% single input keeps its class for the amplitudes
[pks, locs, w, p] = findpeaks(single([0 2 0 1 0]));
asserts.isequal(class(pks), 'single');
asserts.isequal(pks, single([2 1]));
asserts.isequal(locs, [2 4]);
asserts.isequal(class(w), 'double');
asserts.isequal(w, [1 1]);
asserts.isequal(p, [2 1]);
%=============================================================================
% internal helper: an out-of-range peak index has its own error identifier
asserts.checkerror('__findpeaks__(''bases'', [1 2 1], 5)', ...
  sprintf(_('%s must be an index into the signal.'), 'peakIndices'));
asserts.checkerror('__findpeaks__(''bases'', [1 2 1], 5)', ...
  sprintf(_('%s must be an index into the signal.'), 'peakIndices'), 'Nelson:signal:findpeaks:invalidIndex');
%=============================================================================
