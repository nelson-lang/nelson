%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('addtodate'), 3);
assert_isequal(nargout('addtodate'), 1);
%=============================================================================
d = datenum (1974, 8, 15);
assert_isequal(addtodate (d, 0, 'year'), d);
assert_isequal(addtodate (d, 0, 'month'), d);
assert_isequal(addtodate (d, 0, 'day'), d);
assert_isequal(addtodate (d, 0, 'hour'), d);
assert_isequal(addtodate (d, 0, 'minute'), d);
assert_isequal(addtodate (d, 0, 'second'), d);
assert_isequal(addtodate (d, 0, 'millisecond'), d);
%=============================================================================
assert_isequal(addtodate (d, 1, 'year'), d + 365);
assert_isequal(addtodate (d, 1, 'month'), d + 31);
assert_isequal(addtodate (d, 1, 'day'), d + 1);
assert_isequal(addtodate (d, 1, 'hour'), d + (1 / 24));
assert_isequal(addtodate (d, 1, 'minute'), d + (1 / 1440));
assert_isequal(addtodate (d, 1, 'second'), d + (1 / 86400));
assert_isequal(addtodate (d, 1, 'millisecond'), d + (1 / 86400000));
%=============================================================================
assert_isequal(addtodate (d, -1, 'year'), d - 365);
assert_isequal(addtodate (d, -1, 'month'), d - 31);
assert_isequal(addtodate (d, -1, 'day'), d - 1);
assert_isequal(addtodate (d, -1, 'hour'), d - (1 / 24));
assert_isequal(addtodate (d, -1, 'minute'), d - (1 / 1440));
assert_isequal(addtodate (d, -1, 'second'), d - (1 / 86400));
assert_isequal(addtodate (d, -1, 'millisecond'), d - (1 / 86400000));
%=============================================================================
