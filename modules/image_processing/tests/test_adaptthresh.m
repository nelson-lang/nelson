%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('adaptthresh([1 2], -0.1)', sprintf(_('%s must be a finite scalar in the range [%s].'), 'Sensitivity', '0, 1'));
%=============================================================================
asserts.checkerror('adaptthresh([1 2], 0.5, ''NeighborhoodSize'', [2 3])', sprintf(_('%s size must contain positive odd integers.'), 'Neighborhood'));
%=============================================================================
asserts.checkerror('adaptthresh([1 2], 0.5, ''Statistic'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'adaptive statistic'));
%=============================================================================
asserts.checkerror('adaptthresh([1 2], 0.5, ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'adaptthresh'));
