%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('intmin'), -1);
asserts.isequal(nargout('intmin'), -1);
%=============================================================================
asserts.isequal(intmin(), intmin('int32'));
asserts.isequal(intmin('int32'), int32(-2147483648));
asserts.isequal(intmin('int8'), int8(-128));
asserts.isequal(intmin('uint8'), uint8(0));
asserts.isequal(intmin('int16'), int16(-32768));
asserts.isequal(intmin('uint16'), uint16(0));
asserts.isequal(intmin('uint32'), uint32(-2147483648));
asserts.isequal(intmin('int64'), int64(-9223372036854775808));
asserts.isequal(intmin('uint64'), uint64(0));
%=============================================================================
asserts.checkerror('intmin(''uint64'', 3)', _('Too many input arguments.'));
asserts.checkerror('intmin(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
asserts.checkerror('intmin(''t'')', _('The name of an integer class expected.'));
asserts.checkerror('intmin(''t'')', _('The name of an integer class expected.'), 'Nelson:integer:invalidClassName');
asserts.checkerror('[a, b] = intmin()', _('Too many output arguments.'));
%=============================================================================
