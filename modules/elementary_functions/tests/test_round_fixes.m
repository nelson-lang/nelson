%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(round(1.005,2), 1.01);
asserts.isequal(round(1.015,2), 1.02);
asserts.isequal(round(1.025,2), 1.03);
asserts.isequal(round(0.285,2), 0.28999999999999998);
asserts.isequal(round(1234.5678,400), 1234.5678);
asserts.isequal(round(1234.5678,-400), 0);
asserts.isequal(round(1e308,1), 1e+308);
asserts.isequal(round(single(1.005),2), single(1));
asserts.isequal(round(single(2.675),2), single(2.67000008));
asserts.isequal(round(1+1.005i,2), complex(1, 1.01));
asserts.isequal(round(1.0049999999999999,2), 1.01);
asserts.isequal(round(1.255,2), 1.26);
asserts.isequal(round(sparse([1.005 2.675]),2), sparse([1.01 2.6800000000000002]));
asserts.isequal(round(-1.005,2), -1.01);
asserts.isequal(round([1.005 2.675 3.14159],2), [1.01 2.6800000000000002 3.1400000000000001]);
asserts.isequal(round(10.075,2), 10.08);
asserts.isequal(round(5e-324,400), 4.9406564584124654e-324);
asserts.isequal(round(realmax,2), 1.7976931348623157e+308);
asserts.isequal(round(1.5e308,-308), Inf);
asserts.isequal(round(2.5e307,-307), 2.9999999999999998e+307);
asserts.isequal(round(1e-310,310), 9.9999999999999694e-311);
asserts.isequal(round(1e-310,320), 9.9999999999999694e-311);
%=============================================================================
