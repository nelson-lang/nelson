%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nchoosek(5,uint8(2)), uint8(10));
asserts.checkerror('nchoosek(int8(5),uint8(2))', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.checkerror('nchoosek(int8(5),single(2))', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.isequal(nchoosek(60,30), 1.1826458156486142e+17);
asserts.isequal(nchoosek(uint64(60),30), (uint64(118264581)*uint64(1000000000) + uint64(564861424)));
asserts.checkerror('nchoosek(1e17,2)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.checkerror('nchoosek(2^53+2,1)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.isequal(nchoosek('a',2), char(zeros([0 2])));
asserts.checkerror('nchoosek([],2)', _('First input must be a scalar or a vector.'));
asserts.checkerror('nchoosek(uint8(10),int8(3))', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.checkerror('nchoosek(uint16(5),single(2))', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.checkerror('nchoosek(1e16,1)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.isequal(nchoosek(false,0), false([1 0]));
asserts.checkerror('nchoosek(single(1e30),1)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.checkerror('nchoosek(2^53+2,0)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.checkerror('nchoosek(1e16,0)', _('When N is single or double, its maximum allowed value is FLINTMAX.'));
asserts.checkerror('nchoosek(uint8(5),true)', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.checkerror('nchoosek(int8(5),true)', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
asserts.isequal(nchoosek(true,0), false([1 0]));
asserts.isequal(nchoosek(true,2), false([0 2]));
%=============================================================================
