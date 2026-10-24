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
% unary minus of intmin must saturate to intmax
%=============================================================================
asserts.isequal(-int8(-128), int8(127));
asserts.isequal(-int8(127), int8(-127));
asserts.isequal(-intmin('int8'), intmax('int8'));
asserts.isequal(-intmin('int16'), intmax('int16'));
asserts.isequal(-intmin('int32'), intmax('int32'));
asserts.isequal(-intmin('int64'), intmax('int64'));
asserts.isequal(-int8([-128 -1 0 1 127]), int8([127 1 0 -1 -127]));
%=============================================================================
% unsigned types keep saturating at 0
asserts.isequal(-uint8(5), uint8(0));
asserts.isequal(-uint8(0), uint8(0));
%=============================================================================
