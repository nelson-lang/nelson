%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[phalf, fhalf] = pwelch((1:8)', [], [], 8, 4, 'half');
[ponesided, fonesided] = pwelch((1:8)', [], [], 8, 4, 'onesided');
asserts.isapprox(phalf, ponesided, 1e-12);
asserts.isapprox(fhalf, fonesided, 1e-12);
%=============================================================================
[pwhole, fwhole] = pwelch((1:8)', [], [], 8, 4, 'whole');
[ptwosided, ftwosided] = pwelch((1:8)', [], [], 8, 4, 'twosided');
asserts.isapprox(pwhole, ptwosided, 1e-12);
asserts.isapprox(fwhole, ftwosided, 1e-12);
%=============================================================================
asserts.checkerror('pwelch(1:8, [], [], 8, 4, ''bad'')', _('Unsupported option.'));
%=============================================================================
