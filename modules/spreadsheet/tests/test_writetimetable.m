%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2]), [10; 20], 'VariableNames', {'Value'});
filename = [tempname(), '_nelson_writetimetable_direct.xlsx'];
writetimetable(TT, filename, 'Sheet', 'TT', 'WriteMode', 'replacefile');
RTT = readtimetable(filename, 'Sheet', 'TT');
asserts.istrue(istimetable(RTT));
asserts.isequal(RTT.Properties.RowTimes, seconds([1; 2]));
asserts.isequal(RTT.Value, [10; 20]);
rmfile(filename);
%=============================================================================
