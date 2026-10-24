%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[pxx, f] = periodogram([1 + i; 0; 0; 0], [], 4, 2);
asserts.isapprox(pxx, 0.25 * ones(4, 1), 1e-12);
asserts.isapprox(f, [0; 0.5; 1; 1.5], 1e-12);
%=============================================================================
[pxx, f] = periodogram([1 + i; 0; 0; 0], [], 4, 2, 'twosided');
asserts.isapprox(pxx, 0.25 * ones(4, 1), 1e-12);
asserts.isapprox(f, [0; 0.5; 1; 1.5], 1e-12);
%=============================================================================
[pxxHalf, fHalf] = periodogram(ones(8, 1), [], 8, 1, 'half');
[pxxOne, fOne] = periodogram(ones(8, 1), [], 8, 1, 'onesided');
asserts.isapprox(pxxHalf, pxxOne, 1e-12);
asserts.isapprox(fHalf, fOne, 1e-12);
%=============================================================================
[pxxWhole, fWhole] = periodogram(ones(8, 1), [], 8, 1, 'whole');
[pxxTwo, fTwo] = periodogram(ones(8, 1), [], 8, 1, 'twosided');
asserts.isapprox(pxxWhole, pxxTwo, 1e-12);
asserts.isapprox(fWhole, fTwo, 1e-12);
%=============================================================================
asserts.checkerror('periodogram(1:4, [], 4, 1, ''bad'')', _('Unsupported option.'));
%=============================================================================
