%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('detectFASTFeatures([1 2], ''MinContrast'', -1)', ...
  sprintf(_('%s must be a finite scalar in the range [%s].'), 'MinContrast', '0, 1'));
%=============================================================================
asserts.checkerror('detectFASTFeatures([1 2], ''ROI'', [1 1 Inf 2])', sprintf(_('%s must contain finite values.'), 'ROI'));
%=============================================================================
asserts.checkerror('detectFASTFeatures([1 2], ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'feature'));
