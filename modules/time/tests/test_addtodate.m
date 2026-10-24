%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('addtodate'), 3);
asserts.isequal(nargout('addtodate'), 1);
%=============================================================================
d = datenum(1974, 8, 15);
asserts.isequal(addtodate(d, 0, 'year'), d);
asserts.isequal(addtodate(d, 0, 'month'), d);
asserts.isequal(addtodate(d, 0, 'day'), d);
asserts.isequal(addtodate(d, 0, 'hour'), d);
asserts.isequal(addtodate(d, 0, 'minute'), d);
asserts.isequal(addtodate(d, 0, 'second'), d);
asserts.isequal(addtodate(d, 0, 'millisecond'), d);
%=============================================================================
asserts.isequal(addtodate(d, 1, 'year'), d + 365);
asserts.isequal(addtodate(d, 1, 'month'), d + 31);
asserts.isequal(addtodate(d, 1, 'day'), d + 1);
asserts.isequal(addtodate(d, 1, 'hour'), d + (1 / 24));
asserts.isequal(addtodate(d, 1, 'minute'), d + (1 / 1440));
asserts.isequal(addtodate(d, 1, 'second'), d + (1 / 86400));
asserts.isequal(addtodate(d, 1, 'millisecond'), d + (1 / 86400000));
%=============================================================================
asserts.isequal(addtodate(d, -1, 'year'), d - 365);
asserts.isequal(addtodate(d, -1, 'month'), d - 31);
asserts.isequal(addtodate(d, -1, 'day'), d - 1);
asserts.isequal(addtodate(d, -1, 'hour'), d - (1 / 24));
asserts.isequal(addtodate(d, -1, 'minute'), d - (1 / 1440));
asserts.isequal(addtodate(d, -1, 'second'), d - (1 / 86400));
asserts.isequal(addtodate(d, -1, 'millisecond'), d - (1 / 86400000));
%=============================================================================
