%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
pxx = [1 2; 3 4; 5 6];
f = [0; 1; 2];
asserts.isapprox(bandpower(pxx, f, 'psd'), [9 12], 1e-12);
asserts.isapprox(bandpower(pxx, f, [0 1], 'psd'), [4 6], 1e-12);
%=============================================================================
asserts.istrue(isnan(bandpower([1 NaN 3])));
asserts.checkerror('bandpower(1:4, 10)', _('Frequency range must be specified with sampling frequency.'));
%=============================================================================
