%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(x2mdate(1), datenum(1900, 1, 1));
asserts.isequal(x2mdate([2 3]), [datenum(1900, 1, 2) datenum(1900, 1, 3)]);
t = x2mdate(2, 'datetime');
asserts.istrue(isdatetime(t));
asserts.isequal(yyyymmdd(t), 19000102);
%=============================================================================
