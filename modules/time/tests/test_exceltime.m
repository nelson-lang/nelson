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
asserts.isequal(exceltime(datetime(1899, 12, 31)), 0);
asserts.isequal(exceltime(datetime(1900, 1, 1)), 1);
asserts.isequal(exceltime([datetime(1900, 1, 1), datetime(1900, 1, 2)]), [1 2]);
%=============================================================================
% Dates on or after 1900-03-01 include Excel's phantom 1900 leap day.
asserts.isequal(exceltime(datetime(2020, 1, 1)), 43831);
asserts.isequal(exceltime(datetime(1900, 2, 28)), 59);
asserts.isequal(exceltime(datetime(1900, 3, 1)), 61);
asserts.isequal(exceltime([datetime(1900, 2, 28), datetime(1900, 3, 1), datetime(2020, 1, 1)]), [59 61 43831]);
%=============================================================================
% NaT propagates.
asserts.istrue(isnan(exceltime(datetime('NaT'))));
%=============================================================================
% Round-trip through the Excel origin (1899-12-30).
t = datetime(2020, 1, 1);
asserts.isequal(datetime(exceltime(t), 'ConvertFrom', 'excel'), t);
%=============================================================================
