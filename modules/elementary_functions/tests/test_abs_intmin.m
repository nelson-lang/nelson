%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% abs(intmin) saturates to intmax
asserts.isequal(abs(intmin('int8')), intmax('int8'));
asserts.isequal(abs(intmin('int16')), intmax('int16'));
asserts.isequal(abs(intmin('int32')), intmax('int32'));
asserts.isequal(abs(intmin('int64')), intmax('int64'));
% vector (non scalar path)
asserts.isequal(abs([intmin('int8'), int8(-5)]), [intmax('int8'), int8(5)]);
asserts.isequal(abs([intmin('int32'); int32(3)]), [intmax('int32'); int32(3)]);
% unchanged behaviors
asserts.isequal(abs(int8(-5)), int8(5));
asserts.isequal(abs(int8(5)), int8(5));
asserts.isequal(abs(-3.5), 3.5);
%=============================================================================
