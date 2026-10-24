%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('detectHarrisFeatures([1 2], 1, 2)', sprintf(_('%s option name must be a string.'), 'Feature'));
%=============================================================================
asserts.checkerror('detectHarrisFeatures([1 2], ''MinQuality'', 2)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'MinQuality', '0, 1'));
%=============================================================================
asserts.checkerror('detectHarrisFeatures([1 2], ''ROI'', [1 2 3])', ...
  sprintf(_('%s must be a numeric 4-element vector.'), 'ROI'));
