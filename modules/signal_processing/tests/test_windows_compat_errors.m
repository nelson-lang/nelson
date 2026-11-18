%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(rectwin(0), ones(0, 1));
asserts.checkerror('triang(-1)', _('M must be a nonnegative integer.'));
asserts.checkerror('blackmanharris(4, ''bad'')', sprintf(_('%s expected.'), '"periodic" or "symmetric"'));
asserts.checkerror('hann(4, ''bad'')', sprintf(_('%s expected.'), '"periodic" or "symmetric"'));
asserts.checkerror('hamming(4, ''bad'')', sprintf(_('%s expected.'), '"periodic" or "symmetric"'));
asserts.checkerror('blackman(4, ''bad'')', sprintf(_('%s expected.'), '"periodic" or "symmetric"'));
%=============================================================================
