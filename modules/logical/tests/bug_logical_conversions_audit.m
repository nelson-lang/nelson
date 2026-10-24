%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% logical conversions: single support, NaN rejection everywhere
% (2026-07 audit fixes).
%=============================================================================
asserts.isequal(logical(single(1)), true);
asserts.isequal(logical(single([0 2; 3 0])), logical([0 1; 1 0]));
asserts.checkerror('logical(single(NaN))', _('Conversion to logical with NaN is not possible.'));
asserts.checkerror('logical(single(NaN))', _('Conversion to logical with NaN is not possible.'), 'Nelson:logical:nanNotAllowed');
%=============================================================================
asserts.checkerror('xor(NaN, 0)', _('Conversion to logical with NaN is not possible.'));
asserts.checkerror('xor(NaN, 0)', _('Conversion to logical with NaN is not possible.'), 'Nelson:logical:nanNotAllowed');
asserts.checkerror('xor(0, single(NaN))', _('Conversion to logical with NaN is not possible.'));
asserts.isequal(xor(2, 0), true);
%=============================================================================
asserts.checkerror('logical(sparse(NaN))', _('Conversion to logical with NaN is not possible.'));
asserts.isequal(logical(sparse(eye(2))), sparse(logical(eye(2))));
%=============================================================================
