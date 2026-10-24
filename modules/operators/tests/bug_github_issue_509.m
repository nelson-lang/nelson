%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/509
% <-- Short Description -->
% add horzcat vertcat generic support for class object
%=============================================================================
skip_testsuite(isempty(which('weboptions')), 'weboptions is not available.');
%=============================================================================
A = weboptions();
R = [A, A];
asserts.isequal(size(R), [1 2])
asserts.isequal(class(R), 'weboptions')
%=============================================================================
R = [A; A];
asserts.isequal(size(R), [2 1])
asserts.isequal(class(R), 'weboptions')
%=============================================================================
asserts.checkerror('R = [1 2]:3;', _('Colon operands must be real scalars.'));
%=============================================================================
asserts.checkerror('R = ([1 : 2]):3;', _('Colon operands must be real scalars.'));
%=============================================================================
asserts.checkerror('R = [1,2; 3, 4]:3;', _('Colon operands must be real scalars.'));
%=============================================================================
asserts.checkerror('R = [2:4]:[3:5]:[6:7];', _('Colon operands must be real scalars.'));
%=============================================================================
R = []:3;
REF = zeros(1, 0);
asserts.isequal(R, REF);
%=============================================================================
R = 1:2:[];
REF = zeros(1, 0);
asserts.isequal(R, REF);
%=============================================================================
R = 1:[]:4;
REF = zeros(1, 0);
asserts.isequal(R, REF);
%=============================================================================
R = 1:2:zeros(3, 0);
REF = zeros(1, 0);
asserts.isequal(R, REF);
%=============================================================================
