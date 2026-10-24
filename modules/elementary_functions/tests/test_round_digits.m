%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% round(x, n): round to n decimal digits
asserts.isequal(round(2.567, 2), 2.57);
asserts.isequal(round(123.45, -1), 120);
asserts.isequal(round(3.14159, 3), 3.142);
asserts.isequal(round(2.567, 0), 3);
asserts.isequal(round([1.234 5.678], 1), [1.2 5.7]);
asserts.isequal(round(-2.567, 2), -2.57);
% one argument form unchanged
asserts.isequal(round(2.5), 3);
asserts.isequal(round(-2.5), -3);
asserts.isequal(round([1.4 1.6]), [1 2]);
% single
R = round(single(2.567), 2);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), 2.57, 1e-6);
% complex
R = round(1.234 + 5.678i, 1);
asserts.isequal(R, 1.2 + 5.7i);
% errors
asserts.checkerror('round(2.5, 1.5)', _('Second argument must be a real integer scalar.'));
asserts.checkerror('round(int8(5), 2)', _('First argument must be single or double when a second argument is used.'));
%=============================================================================
% significant digits
asserts.isequal(round(12345, 2, 'significant'), 12000);
%=============================================================================
asserts.isequal(round([0.012345 12345 -678.9], 2, 'significant'), [0.012 12000 -680]);
%=============================================================================
asserts.isequal(round(0, 3, 'significant'), 0);
%=============================================================================
asserts.isequal(round(single(12345), 2, 'significant'), single(12000));
%=============================================================================
asserts.isequal(round(3.14159, 3, 'decimals'), 3.142);
%=============================================================================
asserts.checkerror('round(5, 0, ''significant'')', _('With flag ''significant'', the second input must be positive.'));
%=============================================================================
asserts.checkerror('round(5, 2, ''bogus'')', _('Third argument must be ''decimals'' or ''significant''.'));
%=============================================================================
