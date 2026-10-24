%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(compose('%0.2f', [1; 2]), ["1.00"; "2.00"]);
asserts.isequal(compose('%s=%d', "x", 3), "x=3");
asserts.isequal(compose("%% ready"), "% ready");
asserts.isequal(compose('%.7g', single([1.5; pi; 1e10])), ["1.5"; "3.141593"; "1e+10"]);
A = [8 15 9 30; 10 45 11 5];
R = compose("The time is %d:%d", A);
asserts.isequal(size(R), [2 2]);
asserts.isequal(R(1, 1), "The time is 8:15");
asserts.isequal(R(2, 2), "The time is 11:5");
R = compose("Check-in time %d:%d; Check-out time %d:%d", [12 27; 11 16]);
asserts.isequal(R, ["Check-in time 12:27; Check-out time %d:%d"; "Check-in time 11:16; Check-out time %d:%d"]);
%=============================================================================
