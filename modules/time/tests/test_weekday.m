%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('weekday'), -1);
asserts.isequal(nargout('weekday'), -1);
%=============================================================================
[R1, R2] = weekday(728645);
R1_REF = 7;
R2_REF = 'Sat';
asserts.isequal(R1, R1_REF);
asserts.isequal(R2, R2_REF);
%=============================================================================
D = [734999; 735015];
[R1, R2] = weekday(D, 'long');
R1_REF = [5; 7];
R2_REF = ['Thursday'; 'Saturday'];
asserts.isequal(R1, R1_REF);
asserts.isequal(R2, R2_REF);
%=============================================================================
R1_REF = 5;
R2_REF = 'Thu';
R2_LONG_REF = 'Thursday';
%=============================================================================
R1 = weekday('15-Aug-1974');
[R1, R2] = weekday('15-Aug-1974');
asserts.isequal(R1, R1_REF);
asserts.isequal(R2, R2_REF);
%=============================================================================
R1 = weekday('15-Aug-1974', 'long');
[R1, R2] = weekday('15-Aug-1974', 'long');
asserts.isequal(R1, R1_REF);
asserts.isequal(R2, R2_LONG_REF);
%=============================================================================
[R1, R2] = weekday(["15-Aug-1974", "16-Aug-1974"], 'long');
